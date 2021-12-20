from .pyqt_utils import *
from matplotlib.patches import Ellipse, Rectangle, FancyArrow
from matplotlib.collections import PatchCollection
from matplotlib.colors import LogNorm, Normalize
from scipy.interpolate import Rbf
import h5py
import os
from ..utils.file_manager import *
from ..modules.ScanningDiffraction import *
from ..csv_manager import CP_CSVManager
import musclex
import pandas as pd
import numpy as np
from .CPImageWindow import CPImageWindow
import json

matplotlib.rcParams.update({'font.size': 5})

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

        self.browseButton = QPushButton("Browse a HDF or Log File")
        self.browseLogFolderButton = QPushButton("Browse a Log Folder")
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
        self.mainLayout.addWidget(self.browseLogFolderButton, 1, 1, 1, 1)
        self.mainLayout.addWidget(self.createButton, 1, 2, 1, 1)
        self.mainLayout.addWidget(self.browseGrp, 2, 0, 1, 2)
        self.mainLayout.addWidget(self.createGroup, 3, 0, 1, 2)
        self.mainLayout.addWidget(self.bottons,4, 0, 1, 2)

    def setConnection(self):
        """
        Set handler for widgets
        """
        self.browseButton.clicked.connect(self.browseClicked)
        self.browseLogFolderButton.clicked.connect(self.browseLogFolderClicked)
        self.createButton.clicked.connect(self.createClicked)
        self.bottons.accepted.connect(self.okClicked)
        self.bottons.rejected.connect(self.reject)

    def browseClicked(self):
        """
        Handle when Browse is clicked
        """
        self.hdf_file = getAFile('HDF (*.hdf) ;; LOG (*.log)')
        if len(self.hdf_file) > 0:
            self.status = 1
            self.browsedFile.setText(self.hdf_file)
            self.browseGrp.setHidden(False)
            self.createGroup.setHidden(True)

    def browseLogFolderClicked(self):
        """
        Handle when Browse Log Folder is clicked
        """
        self.hdf_file = getAFolder()
        if len(self.hdf_file) > 0:
            self.status = 1
            self.browseLogFolderButton.setText(self.hdf_file)
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
            errMsg.setInformativeText('Please select a HDF or Log file or create a new one')
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
            dihdf={'x_start':x_start,'y_start':y_start, 'x_end':x_end, 'y_end':y_end, 'x_step':x_step,'y_step':y_step}
            filename=fullPath(self.path,'dihdf.json')
            with open(filename,'w') as f:
                json.dump(dihdf,f)

            # The total size divided by the step size is the number of steps in
            # both directions
            # Check if end - start = 0 for either - if it is then run in
            # 1D scan mode?
            if x_step != 0:
                x_nStep = int(np.round((x_end - x_start) / x_step) + 1)
            else:
                x_nStep = 1

            if y_step != 0:
                y_nStep = int(np.round((y_end - y_start) / y_step) + 1)
            else:
                y_nStep = 1

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
        self.max_int = None
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
        self.angle_sigma=1

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
        self.aSigmaLabel = QLabel("Angle Sigma : ")
        self.aSigmaSpnBx = QDoubleSpinBox()
        self.aSigmaSpnBx.setRange(0, 1)
        self.aSigmaSpnBx.setValue(1.0)
        self.aSigmaSpnBx.setDecimals(8)
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
        self.ringSettingsLayout.addWidget(self.aSigmaLabel, 0, 1, 1, 1)
        self.ringSettingsLayout.addWidget(self.distanceLabel, 1, 1, 1, 1)
        self.ringSettingsLayout.addWidget(self.aSigmaSpnBx, 0, 2, 1, 1)
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
        self.repChoice.addItem("Total Intensity (Convex Hull) Map")
        self.repChoice.addItem("Total Intensity Map")
        self.repChoice.addItem("Ring Intensity Map")
        self.repChoice.addItem("D-Space Map")
        self.repChoice.addItem("Vector Field")
        self.repChoice.addItem("Elliptical Map")

        self.colorChoice = QComboBox()
        colormaps = ['jet', 'inferno', 'gray', 'gnuplot', 'gnuplot2', 'hsv', 'magma', 'ocean',
                     'rainbow', 'seismic', 'summer', 'spring', 'terrain', 'winter', 'autumn',
                     'Blues', 'Greens', 'Oranges', 'Reds', 'pink']
        for c in colormaps:
            self.colorChoice.addItem(c)

        self.BSDChoice = QComboBox()
        self.BSDChoice.addItem("None")
        self.BSDChoice.addItem("RBF Interpolation")
        self.BSDChoice.addItem("Alpha Blending")

        self.logScale = QCheckBox('Logarithmic Scale')
        self.rotRAngle = QCheckBox('Rotate 90 degrees')
        self.orientationChkBx = QCheckBox('Orientation Arrow Display')
        self.orientationTypeChkBx = QCheckBox('Arrow Orientation?')

        self.beamXSpinBox = QDoubleSpinBox()
        self.beamYSpinBox = QDoubleSpinBox()
        self.spacingFrame = QFrame()
        self.spacingLayout = QGridLayout(self.spacingFrame)
        self.spacingLayout.addWidget(QLabel("Beam size X : "), 0, 0, 1, 1, Qt.AlignCenter)
        self.spacingLayout.addWidget(self.beamXSpinBox, 0, 1, 1, 1)
        self.spacingLayout.addWidget(QLabel("Beam size Y : "), 0, 2, 1, 1, Qt.AlignCenter)
        self.spacingLayout.addWidget(self.beamYSpinBox, 0, 3, 1, 1)

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
        self.minMapVal = QDoubleSpinBox()
        self.minMapVal.setValue(0)
        self.maxMapVal = QDoubleSpinBox()
        self.scaleX = QDoubleSpinBox()
        self.scaleX.setRange(0.01, 100)
        self.scaleX.setKeyboardTracking(False)
        self.scaleX.setValue(2)
        self.scaleY = QDoubleSpinBox()
        self.scaleY.setRange(0.01, 100)
        self.scaleY.setKeyboardTracking(False)
        self.scaleY.setValue(2)
        self.intensityLayout = QGridLayout()
        self.intensityLayout.addWidget(QLabel("Min Intensity: "), 0, 0, 1, 1, Qt.AlignCenter)
        self.intensityLayout.addWidget(self.minMap, 0, 1, 1, 1)
        self.intensityLayout.addWidget(self.minMapVal, 0, 2, 1, 1)
        self.intensityLayout.addWidget(QLabel("Max Intensity: "), 0, 3, 1, 1, Qt.AlignCenter)
        self.intensityLayout.addWidget(self.maxMap, 0, 4, 1, 1)
        self.intensityLayout.addWidget(self.maxMapVal, 0, 5, 1, 1)
        self.intensityLayout.addWidget(QLabel("Orientation Scale X: "), 0,6, 1, 1, Qt.AlignCenter)
        self.intensityLayout.addWidget(self.scaleX, 0, 7, 1, 1)
        self.intensityLayout.addWidget(QLabel("Orientation Scale Y: "), 1,6, 1, 1, Qt.AlignCenter)
        self.intensityLayout.addWidget(self.scaleY, 1, 7, 1, 1)

        self.mapFigure = plt.figure()
        self.mapAxes = self.mapFigure.add_subplot(111)
        self.mapColorbar = None
        self.mapCanvas = FigureCanvas(self.mapFigure)

        self.saveButton = QPushButton("Save")
        self.savecsvButton=QPushButton("Save CSV (distance +/- bandwidth)")

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
        self.mapLayout.addWidget(QLabel("Beam shape display: "), 4, 0, 1, 1)
        self.mapLayout.addWidget(self.BSDChoice, 4, 1, 1, 2)
        self.mapLayout.addWidget(self.orientationChkBx, 4, 3, 1, 1)
        #self.mapLayout.addWidget(self.orientationTypeChkBx, 4, 4, 1, 1)
        self.mapLayout.addWidget(self.mapCanvas, 5, 0, 1, 4)
        self.mapLayout.addWidget(self.spacingFrame, 6, 0, 1, 4)
        self.mapLayout.addLayout(self.intensityLayout, 7, 0, 2, 4)
        self.mapLayout.addWidget(self.saveButton, 9, 0, 1, 4)
        self.mapLayout.addWidget(self.savecsvButton, 10, 0, 1, 4)
        self.mapLayout.setRowStretch(0, 1)
        self.mapLayout.setRowStretch(1, 1)
        self.mapLayout.setRowStretch(2, 1)
        self.mapLayout.setRowStretch(3, 1)
        self.mapLayout.setRowStretch(4, 1)
        self.mapLayout.setRowStretch(5, 20)
        self.mapLayout.setRowStretch(6, 1)
        self.mapLayout.setRowStretch(7, 1)
        self.mapLayout.setRowStretch(8, 1)

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
        self.orientationChkBx.stateChanged.connect(self.updateUI)
        self.orientationTypeChkBx.stateChanged.connect(self.updateUI)
        self.beamXSpinBox.editingFinished.connect(self.updateUI)
        self.beamYSpinBox.editingFinished.connect(self.updateUI)
        self.img_maxInt.valueChanged.connect(self.maxIntChanged)
        self.img_minInt.valueChanged.connect(self.minIntChanged)

        self.maxMap.valueChanged.connect(self.updateUI)
        self.minMap.valueChanged.connect(self.updateUI)
        self.scaleX.valueChanged.connect(self.updateUI)
        self.scaleY.valueChanged.connect(self.updateUI)
        self.maxMapVal.editingFinished.connect(self.maxMapValChanged)
        self.minMapVal.editingFinished.connect(self.minMapValChanged)

        self.colorChoice.currentIndexChanged.connect(self.updateUI)
        self.repChoice.currentIndexChanged.connect(self.updateUI)
        self.BSDChoice.currentIndexChanged.connect(self.updateUI)
        self.mapFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.saveButton.clicked.connect(self.saveClicked)
        self.savecsvButton.clicked.connect(self.savecsvClicked)
        self.moreDetailsButton.clicked.connect(self.popupImageDetails)

    def ringChoiceChanged(self):
        self.distanceSpnBx.setEnabled(self.distanceRadio.isChecked())
        self.unitChoice.setEnabled(self.distanceRadio.isChecked())
        self.distanceLabel.setEnabled(self.distanceRadio.isChecked())
        self.bandwidthLabel.setEnabled(self.distanceRadio.isChecked())
        self.bandwidthSpnBx.setEnabled(self.distanceRadio.isChecked())

    def maxMapValChanged(self):
        if self.max_int is None:
            return
        self.maxMap.setValue(self.maxMapVal.value() * 100.0 / self.max_int)

    def minMapValChanged(self):
        if self.max_int is None:
            return
        self.minMap.setValue(self.minMapVal.value() * 100.0 / self.max_int)

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
        name, extension = os.path.splitext(filename)
        if extension == 'svg':
            self.mapFigure.savefig(filename, format='svg')
        elif extension == 'png':
            self.mapFigure.savefig(filename, format='png')
        else:
            self.mapFigure.savefig(filename)
        print(str(filename)+" has been saved.")

    def plotClicked(self, event):
        self.moreDetailsButton.setHidden(False)

        if len(self.xyIntensity) < 3 or event.xdata is None or event.ydata is None:
            return

        x = self.xyIntensity[0]
        y = self.xyIntensity[1]
        stepx, stepy = tuple(self.xylim)
        x_max = len(x)

        # if a 1D scan then scale according to varying axis
        if len(x) == 1:
            if x[0] == 0:
                stepx = stepy/2
        if len(y) == 1:
            if y[0] == 0:
                stepy = stepx/2

        if x.min()-stepx/2 <= event.xdata <= x.max()+stepx/2 and y.min()-stepy/2 <= event.ydata <= y.max()+stepy/2:
            col = min(np.arange(len(x)), key=lambda i: abs(x[i] - event.xdata))
            row = min(np.arange(len(y)), key=lambda i: abs(y[i] - event.ydata))

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
            ax.text(cx, cy, "Please click on map\nto see the image\n and details", horizontalalignment='center')

        self.imgFigure.subplots_adjust(left=0.15, bottom=0.25, right=0.85, top=0.90, wspace=0, hspace=0)
        self.imgCanvas.draw()

    def mousePressEvent(self, event):
        focused_widget = QApplication.focusWidget()
        if focused_widget != None:
            focused_widget.clearFocus()

    def updateUI(self):
        QApplication.setOverrideCursor(Qt.WaitCursor)
        self.colormap = str(self.colorChoice.currentText())
        self.usingLogScale = self.logScale.isChecked()
        self.rotating90 = self.rotRAngle.isChecked()
        self.spacingFrame.setVisible(self.BSDChoice.currentIndex() == 2)
        representation = self.repChoice.currentText()
        if representation == 'Total Intensity (Convex Hull) Map':
            self.updateIntensityMap(type='hull')
        if representation == 'Total Intensity Map':
            self.updateIntensityMap(type='simple')
        elif representation == 'Ring Intensity Map':
            self.updateIntensityMap(type='ring')
        elif representation == 'Vector Field':
            self.updateVectorFieldMap()
        elif representation == 'Elliptical Map':
            self.updateEllipticalMap()
        elif representation == 'D-Space Map':
            self.updateIntensityMap(type='dspace')
        # elif selected_tab == 1:
        #     self.updateAngularRangeTab()
      
        if self.max_int is not None:
            self.minMapVal.setMaximum(self.max_int)
            self.maxMapVal.setMaximum(self.max_int)
            self.minMapVal.setValue(self.minMap.value() / 100.0 * self.max_int)
            self.maxMapVal.setValue(self.maxMap.value() / 100.0 * self.max_int)
        if self.orientationChkBx.isChecked():
            self.orientationTypeChkBx.setChecked(True)
      
        
        QApplication.processEvents()
        QApplication.restoreOverrideCursor()


    def updateIntensityMap(self, type='simple'):
        if len(self.xyIntensity) < 3:
            return
        rendering_mode = self.BSDChoice.currentIndex()
        self.orientationChkBx.setEnabled(True)
        self.orientationTypeChkBx.setEnabled(True)
        angle = self.orientationChkBx.isChecked()

        stepx, stepy = self.xylim[0], self.xylim[1]
        # if one or the other is 0, this is a 1D scan
        if stepx == 0:
            stepx = stepy / 2
        if stepy == 0:
            stepy = stepx / 2

        beamx, beamy = self.beamXSpinBox.value(), self.beamYSpinBox.value()
        x = copy.copy(self.xyIntensity[0])
        y = copy.copy(self.xyIntensity[1])
        x2 = np.append(x, max(x) + stepx) - stepx / 2
        y2 = np.append(y, max(y) + stepy) - stepy / 2
        if type == 'hull':
            intensity = copy.copy(self.xyIntensity[2])
        elif type == 'ring':
            intensity = copy.copy(self.xyIntensity[3])
        elif type == 'simple':
            intensity = copy.copy(self.xyIntensity[4])
        elif type == 'dspace':
            intensity = copy.copy(self.xyIntensity[5])

        x_coor, y_coor = np.meshgrid(x2, y2)

        max_val = intensity.max()
        self.max_int = max_val
        if self.maxMap.value() < 100:
            intensity[
                intensity > self.maxMap.value() * max_val / 100.] = self.maxMap.value() * max_val / 100.
        if self.minMap.value() > 0:
            intensity[
                intensity < self.minMap.value() * max_val / 100.] = self.minMap.value() * max_val / 100.
        max_val = intensity.max()
        min_val = intensity[intensity > 0].min()

        xlim = (x.min() - stepx, x.max() + stepx)
        ylim = (y.min() - stepy, y.max() + stepy)

        ax = self.mapAxes
        ax.cla()

        if self.minMapVal.value() == 0 and self.maxMapVal.value() == 0: # on initialization
            norm = LogNorm(vmin=min_val+1e-6, vmax=max_val) if self.usingLogScale else \
                   Normalize(vmin=0, vmax=max_val)
        else:
            norm = LogNorm(vmin=self.minMapVal.value()+1e-6, vmax=self.maxMapVal.value()) if self.usingLogScale else \
                   Normalize(vmin=self.minMapVal.value(), vmax=self.maxMapVal.value())

        im = ax.pcolormesh(x_coor, y_coor, intensity, cmap=self.colormap, norm=norm)
        
        # self.intensityMapFigure.colorbar(im)
        if rendering_mode == 1: # RBF Interpolation
            ax.cla()
            colorm = matplotlib.cm.ScalarMappable(cmap=self.colormap, norm=norm).to_rgba(intensity)
            xi, yi = np.meshgrid(x, y)
            rbf_r = Rbf(xi, yi, colorm[:,:,0], epsilon=(beamx+beamy)/4)
            rbf_g = Rbf(xi, yi, colorm[:,:,1], epsilon=(beamx+beamy)/4)
            rbf_b = Rbf(xi, yi, colorm[:,:,2], epsilon=(beamx+beamy)/4)
            XI, YI = np.meshgrid(np.linspace(xlim[0], xlim[1], len(x)*5), np.linspace(ylim[0], ylim[1], len(y)*5))
            ZI = np.zeros((XI.shape[0], XI.shape[1], 3))
            ZI[:,:,0] = rbf_r(XI, YI)
            ZI[:,:,1] = rbf_g(XI, YI)
            ZI[:,:,2] = rbf_b(XI, YI)
            ax.imshow(ZI, origin='lower', extent=[xlim[0], xlim[1], ylim[0], ylim[1]], aspect=4)

        elif rendering_mode == 2: # Alpha blending
            ax.cla()
            if stepx >= beamx and stepy >= beamy:
                patches, colors = [], []
                for j in range(len(x)):
                    for i in range(len(y)):
                        rect = Rectangle(xy=(x[j]-beamx/2, y[i]-beamy/2), width=beamx, height=beamy)
                        patches.append(rect)
                        colors.append(-1 if intensity.mask[i, j] else intensity[i, j])
                pc = PatchCollection(patches, cmap=self.colormap, norm=norm, antialiaseds=False)
                pc.set_array(np.ma.array(colors, mask=np.array(colors) < 0))
                ax.add_collection(pc)
            else:
                patchesl = [[], [], [], []]
                intensityl = [[], [], [], []]
                for j in range(len(x)):
                    for i in range(len(y)):
                        rect = Rectangle(xy=(x[j]-beamx/2, y[i]-beamy/2), width=beamx, height=beamy)
                        grp = 2 * (i % 2) + (j % 2)
                        patchesl[grp].append(rect)
                        intensityl[grp].append(-1 if intensity.mask[i, j] else intensity[i, j])
                tmpfig, tmpax = plt.subplots(figsize=(12.0, 12.0 / (len(x) + 1) * (len(y) + 1)))
                tmpfig.subplots_adjust(left=0, bottom=0, right=1, top=1, wspace=0, hspace=0)
                imgs = []
                h, w = None, None
                for i in range(len(patchesl)):
                    p = PatchCollection(patchesl[i], cmap=self.colormap, norm=norm, antialiaseds=False)
                    p.set_array(np.ma.array(intensityl[i], mask=np.array(intensityl[i]) < 0))
                    tmpax.cla()
                    tmpax.add_collection(p)
                    tmpax.set_xlim(*xlim)
                    tmpax.set_ylim(*ylim)
                    tmpax.set_axis_off()
                    tmpfig.canvas.draw()
                    if h is None or w is None:
                        w, h = tmpfig.canvas.get_width_height() # width and height
                    imgs.append(np.frombuffer(tmpfig.canvas.tostring_rgb(), np.uint8).reshape(h, w, -1))

                sx, sy = w / (len(x) + 1), h / (len(y) + 1) # step size in pixel
                bx, by = sx * beamx / stepx, sy * beamy / stepy # beam size in pixel
                def blend(imga, imgb, ori='h'):
                    img = imga.copy()
                    for idx in np.ndindex(h, w):
                        if imga[idx].sum() >= 765:
                            img[idx] = imgb[idx]
                        elif imgb[idx].sum() < 765:
                            if ori != 'v':
                                rx = idx[1] % sx
                                d0, d1 = rx - (sx - bx / 2), bx / 2 - rx
                                w1, w2 = (d0, d1) if (idx[1] // sx) % 2 == 0 else (d1, d0)
                            else:
                                ry = idx[0] % sy
                                d0, d1 = ry - (sy - by / 2), by / 2 - ry
                                w1, w2 = (d1, d0) if (idx[0] // sy) % 2 == 0 else (d0, d1)
                            w1 = w1 if w1 > 0 else 0
                            w2 = w2 if w2 > 0 else 0
                            w1, w2 = w1 / (w1 + w2), w2 / (w1 + w2)
                            img[idx] = np.array(imga[idx] * w1 + imgb[idx] * w2, np.uint8)
                    return img
                img = blend(blend(imgs[0], imgs[1]), blend(imgs[2], imgs[3]), ori='v')
                ax.imshow(img, origin='upper', extent=[xlim[0], xlim[1], ylim[0], ylim[1]], aspect=4)

        if angle: # orientation display for drawing ellipses
            centers = [(x[i], y[j]) for j in range(len(y)) for i in range(len(x))]
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
                    if self.orientationTypeChkBx.isChecked():
                        e = FancyArrow(x=centers[i][0], y=centers[i][1], dx=0, dy=0)
                    else:
                        e = Ellipse(xy=centers[i], width=(stepx + stepy)/2./self.scaleX.value(), height=(stepx + stepy)/2./self.scaleY.value())
                else:
                    if self.orientationTypeChkBx.isChecked():
                        dx = min(stepx,stepy)/self.scaleY.value() * np.cos(angle_factor * self.orientation_dict[i + self.init_number])
                        dy = min(stepx,stepy)/self.scaleY.value() * np.sin(angle_factor * self.orientation_dict[i + self.init_number])
                        if self.rotating90:
                            tempx=dx
                            tempy=dy
                            dx=tempy
                            dy=-tempx   
                        e = FancyArrow(x=centers[i][0], y=centers[i][1], dx=dx, dy=dy)
                    else:
                        e_angle = convertRadtoDegreesEllipse((0 if self.rotating90 else np.pi/2.) +
                            angle_factor * self.orientation_dict[i + self.init_number])
                        e = Ellipse(xy=centers[i], width=(stepx + stepy)/2./self.scaleX.value(), height=(stepx + stepy)/2./self.scaleY.value(), angle=e_angle)

                patches.append(e)
                c = max_val - self.intensity_dict[i + self.init_number] if i + self.init_number in self.intensity_dict else -1
                colors.append(c)
                # colors.append(0)

            colors = np.ma.array(colors, mask=np.array(colors) < 0)
            try:
                min_val = colors[colors > 0].min()
            except ValueError:
                min_val = -1
            norm = LogNorm(vmin=min_val, vmax=max_val) if self.usingLogScale else None
            #p = PatchCollection(patches, cmap=self.colormap, norm=norm)
            # p = PatchCollection(patches, cmap='gray', norm=norm)
            #p.set_array(colors)
            #ax.add_collection(p)
            for patch in patches:
                patch.set_color('black')
                ax.add_patch(patch)

        ax.set_xlim(*xlim)
        ax.set_ylim(*ylim)
        # ax.set_aspect('auto') # will alter the aspect ratio with window resizing
        ax.set_aspect('equal') # makes plot aspect ratio static
        ax.set_adjustable('datalim', share=True)
        ax.set_facecolor('white')

        if self.isFlipX:
            ax.invert_xaxis()
        if self.isFlipY:
            ax.invert_yaxis()

        if self.mapColorbar is None:
            self.mapColorbar = self.mapFigure.colorbar(im, ax=ax, fraction=0.08, pad=0.01)
        else:
            self.mapFigure.axes[1].cla()
            self.mapFigure.colorbar(im, cax=self.mapColorbar.ax)

        self.mapFigure.subplots_adjust(left=0.125, bottom=0.2, right=0.9, top=0.9,wspace=0, hspace=0)
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
        self.orientationChkBx.setChecked(False)
        self.orientationTypeChkBx.setChecked(True)
        self.orientationChkBx.setEnabled(False)
        self.orientationTypeChkBx.setEnabled(False)

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
        self.max_int = max_val
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

        # scale units determine the length of the vector arrows
        # for 1d scans, it should scale in the variable direction only
        if len(x) == 1:
            scale_units = 'y'
        elif len(y) == 1:
            scale_units = 'x'
        else:
            scale_units = 'xy'

        self.vec_quiver = ax.quiver(x, y, UN, VN,  # data
                                    int_display,  # colour the arrows based on this array
                                    cmap=self.colormap, norm=norm, # colour map
                                    headlength=7, headwidth=4, scale_units=scale_units, pivot='middle')
        # if one of these is 0, it's a 1D scan. set limits on the scale of the non-zero step size
        if (self.xylim[0]/2 > 0):
            ax.set_xlim(x.min() - self.xylim[0], x.max() + self.xylim[0])
        else:
            ax.set_xlim(x.min() - self.xylim[1], x.max() + self.xylim[1])
        if (self.xylim[1]/2 >0):
            ax.set_ylim(y.min() - self.xylim[1], y.max() + self.xylim[1])
        else:
            ax.set_ylim(y.min() - self.xylim[0], y.max() + self.xylim[0])
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
            self.mapFigure.axes[1].cla()
            self.mapFigure.colorbar(self.vec_quiver, cax=self.mapColorbar.ax)
        self.mapFigure.subplots_adjust(left=0.125, bottom=0.2, right=0.9, top=0.9, wspace=0, hspace=0)
        self.mapCanvas.draw()

        # if self.arrowLengthSlider.value() > 5:
        #     self.updateVectorFieldArrow()

    def updateVectorFieldArrow(self):
        if len(self.xyIntensity) < 3 or len(self.vec_UV) < 2 or self.vec_quiver is None:
            return
        self.orientationChkBx.setChecked(False)
        self.orientationTypeChkBx.setChecked(False)
        self.orientationChkBx.setEnabled(False)
        self.orientationTypeChkBx.setEnabled(False)

        intensity = self.xyIntensity[2]
        U = self.vec_UV[0]
        V = self.vec_UV[1]
        int_display = copy.copy(intensity)

        max_val = int_display.max()
        self.max_int = max_val
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
        self.orientationChkBx.setChecked(False)
        self.orientationTypeChkBx.setChecked(False)
        self.orientationChkBx.setEnabled(False)
        self.orientationTypeChkBx.setEnabled(False)

        if len(self.xyIntensity) < 3:
            return

        x = self.xyIntensity[0]
        y = self.xyIntensity[1]

        centers = [(x[i], y[j]) for j in range(len(y)) for i in range(len(x))]
        ranges = [toFloat(self.angrange_dict[i]) if i in self.angrange_dict.keys() else 0. for i in
                  range(self.init_number, len(self.hdf_data) + self.init_number)]
        max_width = max(ranges) * 5.
        widths = [toFloat(self.angrange_dict[i]) / max_width if i in self.angrange_dict.keys() and 0 < toFloat(
            self.angrange_dict[i]) / max_width else max_width / 2. for i in
                  range(self.init_number, len(self.hdf_data) + self.init_number)]

        int_display = np.array(list(self.intensity_dict.values()))
        max_val = int_display.max()
        self.max_int = max_val
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
        if (self.xylim[0]/2 > 0):
            ax.set_xlim(x.min() - self.xylim[0]/2, x.max() + self.xylim[0]/2)
        else:
            ax.set_xlim(x.min() - self.xylim[1]/2, x.max() + self.xylim[1]/2)
        if (self.xylim[1]/2 >0):
            ax.set_ylim(y.min() - self.xylim[1]/2, y.max() + self.xylim[1]/2)
        else:
            ax.set_ylim(y.min() - self.xylim[0]/2, y.max() + self.xylim[0]/2)
        ax.set_aspect('auto')

        if self.isFlipX:
            ax.invert_xaxis()
        if self.isFlipY:
            ax.invert_yaxis()

        if self.mapColorbar is None:
            self.mapColorbar = self.mapFigure.colorbar(p, ax=ax, fraction=0.08, pad=0.01)
        else:
            self.mapFigure.axes[1].cla()
            self.mapFigure.colorbar(p, cax=self.mapColorbar.ax)
        self.mapFigure.subplots_adjust(left=0.125, bottom=0.2, right=0.9, top=0.9, wspace=0, hspace=0)
        # self.mapFigure.savefig(fullPath(self.filePath, 'cp_results/vector_field.png'))
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
    def savecsvClicked(self):
        df_rings = self.csvManager.df_rings
        distance=self.distanceSpnBx.value()
        max_diff=self.bandwidthSpnBx.value()
        minv=distance-max_diff
        maxv=distance+max_diff
        df_rings=df_rings.loc[(df_rings['d']>=minv) &(df_rings['d']<=maxv)]
        self.angle_sigma=self.aSigmaSpnBx.value()
        df_rings=df_rings[df_rings['angle sigma']<self.angle_sigma]
        dir_path=self.filePath
        unit = str(self.unitChoice.currentText())
        csvname=f'rings_({minv}-{maxv}){unit}_anglesigma{self.angle_sigma}.csv'
        csvfilepath=os.path.join(dir_path,'cp_results',csvname)
        print(df_rings.shape)
        filename=getSaveFile(csvfilepath,None)
        if filename!="":
            df_rings.to_csv(filename,index=False)


        
        

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
        self.angle_sigma=1
        self.angle_sigma=self.aSigmaSpnBx.value()
        
        df_rings=df_rings[df_rings['angle sigma']<self.angle_sigma]
        filelist=df_rings['filename'].tolist()
        df_sum=df_sum[df_sum['filename'].isin(filelist)]

        

        # Read intensity from csv to organize the info given
        self.name_dict = {}
        self.intensity_dict = {}
        self.sim_inten_dict = {}
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
            self.intensity_dict[index] = row['total intensity (hull)'] \
                    if 'total intensity (hull)' in row and not np.isnan(row['total intensity (hull)']) else \
                    row['total intensity']
            self.sim_inten_dict[index] = row['total intensity']
            #print(np.isnan(row['total intensity (hull)']), self.intensity_dict[index], self.sim_inten_dict[index])

            # Add ring model if its error < 1. and sigma < 1. (prevent uniform ring)
       
            all_rings = df_rings[(df_rings['filename'] == filename) &(df_rings['angle sigma']<self.angle_sigma)]
            

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
                
                good_model = float(best_ring['angle fitting error']) < 1. and best_ring['angle sigma'] < self.angle_sigma and distance_ok
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
        self.hdf_data = self.get_scan_data(hdf_filename)

        self.coord_dict = {}
        
        for i in range(self.init_number, len(self.hdf_data) + self.init_number):
            self.coord_dict[i] = (self.hdf_data[i - self.init_number][0], self.hdf_data[i - self.init_number][1])
        nCols = len(self.hdf_data) # 1D Scan
        for i in range(self.init_number + 1, len(self.hdf_data) + self.init_number):
            if abs(self.coord_dict[i][1] - self.coord_dict[i - 1][1]) != 0:
                nCols = i - self.init_number
                break
        if nCols != 0:
            nRows = int(len(self.hdf_data) / nCols)
        else :
            nRows = 0
        all_xs = np.reshape(np.array([v[0] for k, v in self.coord_dict.items()]), (nRows, nCols))
        all_ys = np.reshape(np.array([v[1] for k, v in self.coord_dict.items()]), (nRows, nCols))
        x = np.mean(all_xs, axis=0)
        y = np.mean(all_ys, axis=1)

        if len(x) > 1:
            x_grad = abs(x[1] - x[0])
        else:
            x_grad = 0
        if len(y) > 1:
            y_grad = abs(y[1] - y[0])
        else:
            y_grad = 0
       
        # Plot heatmap for intensity
        z = [float(self.intensity_dict[i]) if i in self.intensity_dict else -1 for i in
             range(self.init_number, len(self.hdf_data) + self.init_number)]
        simp_z = [float(self.sim_inten_dict[i]) if i in self.sim_inten_dict else -1 for i in
             range(self.init_number, len(self.hdf_data) + self.init_number)]
        dist_z = [float(self.distance_dict[i]) if i in self.distance_dict else -1 for i in
             range(self.init_number, len(self.hdf_data) + self.init_number)]
        # z = np.array([z[i:i + x_max] for i in range(0, , x_max)])
        # z = cv2.blur(z, (4,4))
        # intensity = np.array(z)
        ring_z = [float(self.peak_intensity_dict[i]) if i in self.peak_intensity_dict else -1 for i in
             range(self.init_number, len(self.hdf_data) + self.init_number)]
        intensity = np.reshape(z, (len(y), len(x)))
        simp_intensity = np.reshape(simp_z, (len(y), len(x)))
        ring_intensity = np.reshape(ring_z, (len(y), len(x)))
        intensity = np.ma.array(intensity, mask=intensity < 0)
        simp_intensity = np.ma.array(simp_intensity, mask=simp_intensity < 0)
        ring_intensity = np.ma.array(ring_intensity, mask=ring_intensity < 0)
        dspace_intensity = np.reshape(dist_z, (len(y), len(x)))

        self.xyIntensity = [x, y, intensity, ring_intensity, simp_intensity, dspace_intensity]
        self.beamXSpinBox.setValue(x_grad)
        self.beamXSpinBox.setMaximum(x_grad * 2)
        self.beamYSpinBox.setValue(y_grad)
        self.beamYSpinBox.setMaximum(y_grad * 2)
        self.xylim = [self.beamXSpinBox.value(), self.beamYSpinBox.value()]
        self.refreshAllTabs()
        self.updateImage()
        self.updateUI()
        QApplication.restoreOverrideCursor()

    def convert_to_float(self, i):
        return float(i) if i.replace('.', '', 1).replace('-', '').isdigit() else i

    def get_scan_data(self, filename):
        if h5py.is_hdf5(filename):
            hf = h5py.File(filename, 'r')
            coordinates=np.array(hf.get('data').get('BL'))
            if 'header' in hf.keys():
                x_initial=hf.get('header').get('Xinitial')[0]
                y_initial=hf.get('header').get('Yinitial')[0]
                x_final=hf.get('header').get('Xfinal')[0]
                y_final=hf.get('header').get('Yfinal')[0]
                if x_initial>x_final:
                    xs=-1
                else:
                    xs=1
                if y_initial>y_final:
                    ys=-1
                else:
                    ys=1
                # x_step=hf.get('header').get('Xstep')[0]
                # y_step=hf.get('header').get('Ystep')[0]
            
                # if x_initial>x_final:
                #     x_step=-1*x_step
                # if y_initial>y_final:
                #     y_step=-1*y_step
                
                # xnstep=math.ceil((x_final-x_initial)/x_step)
                # ynstep=math.ceil((y_final-y_initial)/y_step)
                # print(x_initial)
                # print(x_final)
                # print(xnstep)
                # print(y_initial)
                # print(y_final)
                # print(ynstep)
                # data=[]
                # x_initial=x_final
                # y_initial=y_final
                # x_step=1
                # y_step=1
                
                # for j in range(0,ynstep):
                #     y=y_initial+j*y_step
                #     for i in range(0,xnstep):
                #         x=x_initial+i*x_step
                #         data.append((x,y))
                # coordinates=np.array(data)
                coordinates=np.array(hf.get('data').get('BL'))
                
                l=len(coordinates)
                coor=[]

                for i in range(0,l):
                    coor.append((xs*coordinates[i][0],ys*coordinates[i][1]))
                coordinates=np.array(coor)


            return coordinates
        elif os.path.isdir(filename):
            return sorted(self.parse_logfiles_dir(filename), key=lambda x: (x[1], x[0]))
        elif filename.endswith('.log'):
            return self.parse_logfile(filename)

    def parse_logfile(self, filename):
        data_dir, fname = os.path.split(filename)
        count_filename = os.path.join(data_dir, fname)

        with open(count_filename, 'r') as f:
            all_lines = f.readlines()

        line_num = 0
        for i, line in enumerate(all_lines):
            if not line.startswith('#'):
                line_num = i
                break

        headers = all_lines[line_num - 1].replace('\n', '').split('\t')
        x_index = headers.index('x')
        y_index = headers.index('y')

        print(f'Log Headers: {headers},\n x index: {x_index},\n y index: {y_index}')
        scans = []
        for i in range(line_num, len(all_lines)):
            data = all_lines[i].replace('\n', '').split('\t')
            scans.append(list(map(self.convert_to_float, [data[x_index], data[y_index]])))
        return scans

    def parse_logfiles_dir(self, dir_name):
        files = os.listdir(dir_name)
        data = []
        for f in files:
            if f.endswith('.log'):
                scans = self.parse_logfile(os.path.join(dir_name, f))
                data.extend(scans)
        return data


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

