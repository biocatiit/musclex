from .pyqt_utils import *
from matplotlib.patches import Ellipse
from matplotlib.collections import PatchCollection
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
        self.y_start = QDoubleSpinBox()
        self.y_start.setRange(-100000, 100000)
        self.x_end = QDoubleSpinBox()
        self.x_end.setRange(-100000, 100000)
        self.y_end = QDoubleSpinBox()
        self.y_end.setRange(-100000, 100000)
        self.x_step = QDoubleSpinBox()
        self.x_step.setRange(-100000, 100000)
        self.y_step = QDoubleSpinBox()
        self.y_step.setRange(-100000, 100000)

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
        self.update_plot = {'intensity_maps': True,
                            'ds_maps': True,
                            'arange_maps': True,
                            'vector_maps': True,
                            'ellipse_maps': True
                            }
        self.intesityRange = [0, 1, 1, 2]
        self.mainWin = mainWin
        self.color_maps = 'jet'

        ##Batch Mode Params
        self.stopProcess = False
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
        self.setWindowTitle("Scanning Diffraction v." + musclex.__version__)
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
        self.imgFrame.setHidden(True)

        #### BATCHMODE - tabs
        ## BATCHMODE 1 : Total Intensity Map Tab
        self.intensityTab = QWidget()
        self.intensityTab.setContentsMargins(0, 0, 0, 0)
        self.intensityTabLayout = QGridLayout(self.intensityTab)
        self.intensityMapFigure = plt.figure()
        self.intensityMapAxes = self.intensityMapFigure.add_subplot(111)
        self.intensityMapCanvas = FigureCanvas(self.intensityMapFigure)
        self.intensityMapFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.int_maxIntMap = QDoubleSpinBox()
        self.int_maxIntMap.setMinimum(1)
        self.int_maxIntMap.setMaximum(100)
        self.int_maxIntMap.setValue(100.)
        self.int_maxIntMap.setSingleStep(5.0)
        self.int_maxIntMap.setSuffix("%")
        self.int_maxIntMap.setKeyboardTracking(False)

        self.int_minIntMap = QDoubleSpinBox()
        self.int_minIntMap.setMinimum(0)
        self.int_minIntMap.setMaximum(99)
        self.int_minIntMap.setValue(0)
        self.int_minIntMap.setSingleStep(5.0)
        self.int_minIntMap.setSuffix("%")
        self.int_minIntMap.setKeyboardTracking(False)

        self.int_MapIntSettings = QHBoxLayout()
        self.int_MapIntSettings.addWidget(QLabel("Max : "))
        self.int_MapIntSettings.addWidget(self.int_maxIntMap)
        self.int_MapIntSettings.addWidget(QLabel("Min : "))
        self.int_MapIntSettings.addWidget(self.int_minIntMap)
        self.intensityTabLayout.addWidget(self.intensityMapCanvas, 0, 0, 1, 1)
        self.intensityTabLayout.addLayout(self.int_MapIntSettings, 1, 0, 1, 1)

        ## BATCHMODE 2 : Distance btw rings map
        self.distanceMapTab = QWidget()
        self.distanceMapTab.setContentsMargins(0, 0, 0, 0)
        self.distanceMapTabLayout = QGridLayout(self.distanceMapTab)
        self.distanceMapFigure = plt.figure()
        self.distanceMapAxes = self.distanceMapFigure.add_subplot(111)
        self.distanceMapCanvas = FigureCanvas(self.distanceMapFigure)
        self.distanceMapFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.ds_ImgFigure = plt.figure()
        self.ds_ImgCanvas = FigureCanvas(self.ds_ImgFigure)

        self.ds_maxIntMap = QDoubleSpinBox()
        self.ds_maxIntMap.setMinimum(1)
        self.ds_maxIntMap.setMaximum(100)
        self.ds_maxIntMap.setValue(100.)
        self.ds_maxIntMap.setSingleStep(5.0)
        self.ds_maxIntMap.setSuffix("%")
        self.ds_maxIntMap.setKeyboardTracking(False)
        self.ds_minIntMap = QDoubleSpinBox()
        self.ds_minIntMap.setMinimum(0)
        self.ds_minIntMap.setMaximum(99)
        self.ds_minIntMap.setValue(0)
        self.ds_minIntMap.setSingleStep(5.0)
        self.ds_minIntMap.setSuffix("%")
        self.ds_minIntMap.setKeyboardTracking(False)
        self.ds_MapIntSettings = QHBoxLayout()
        self.ds_MapIntSettings.addWidget(QLabel("Max : "))
        self.ds_MapIntSettings.addWidget(self.ds_maxIntMap)
        self.ds_MapIntSettings.addWidget(QLabel("Min : "))
        self.ds_MapIntSettings.addWidget(self.ds_minIntMap)
        self.distanceMapTabLayout.addWidget(self.distanceMapCanvas, 0, 0, 1, 1)
        self.distanceMapTabLayout.addLayout(self.ds_MapIntSettings, 1, 0, 1, 1)

        ## BATCHMODE 3 : Angular range map (degrees)
        self.angularMapTab = QWidget()
        self.angularMapTab.setContentsMargins(0, 0, 0, 0)
        self.angularMapTabLayout = QGridLayout(self.angularMapTab)
        self.angularMapTab = QWidget()
        self.angularMapTab.setContentsMargins(0, 0, 0, 0)
        self.angularMapTabLayout = QGridLayout(self.angularMapTab)
        self.angularMapFigure = plt.figure()
        self.angularMapAxes = self.angularMapFigure.add_subplot(111)
        self.angularMapCanvas = FigureCanvas(self.angularMapFigure)
        self.angularMapFigure.canvas.mpl_connect('button_press_event', self.plotClicked)

        self.ar_maxIntMap = QDoubleSpinBox()
        self.ar_maxIntMap.setMinimum(1)
        self.ar_maxIntMap.setMaximum(100)
        self.ar_maxIntMap.setValue(100.)
        self.ar_maxIntMap.setSingleStep(5.0)
        self.ar_maxIntMap.setSuffix("%")
        self.ar_maxIntMap.setKeyboardTracking(False)
        self.ar_minIntMap = QDoubleSpinBox()
        self.ar_minIntMap.setMinimum(0)
        self.ar_minIntMap.setMaximum(99)
        self.ar_minIntMap.setValue(0)
        self.ar_minIntMap.setSingleStep(5.0)
        self.ar_minIntMap.setSuffix("%")
        self.ar_minIntMap.setKeyboardTracking(False)
        self.ar_MapIntSettings = QHBoxLayout()
        self.ar_MapIntSettings.addWidget(QLabel("Max : "))
        self.ar_MapIntSettings.addWidget(self.ar_maxIntMap)
        self.ar_MapIntSettings.addWidget(QLabel("Min : "))
        self.ar_MapIntSettings.addWidget(self.ar_minIntMap)
        self.angularMapTabLayout.addWidget(self.angularMapCanvas, 0, 0, 1, 1)
        self.angularMapTabLayout.addLayout(self.ar_MapIntSettings, 1, 0, 1, 1)

        ## BATCHMODE 4 : Vector Fields
        self.vectorFieldTab = QWidget()
        self.vectorFieldTab.setContentsMargins(0, 0, 0, 0)
        self.vectorFieldTabLayout = QGridLayout(self.vectorFieldTab)
        self.vectorFieldMapFigure = plt.figure()
        self.vectorFieldMapAxes = self.vectorFieldMapFigure.add_subplot(111)
        self.vectorFieldMapCanvas = FigureCanvas(self.vectorFieldMapFigure)
        self.vectorFieldMapFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.arrowLengthSlider = QSlider()
        self.arrowLengthSlider.setMinimum(5)
        self.arrowLengthSlider.setMaximum(25)
        self.arrowLengthSlider.setValue(5)
        self.arrowLengthSlider.setOrientation(Qt.Horizontal)

        self.vectorFieldTabLayout.addWidget(self.vectorFieldMapCanvas, 0, 0, 1, 2)
        self.vectorFieldTabLayout.addWidget(QLabel("Arrow Length"), 1, 0, 1, 1)
        self.vectorFieldTabLayout.addWidget(self.arrowLengthSlider, 1, 1, 1, 1)

        ## BATCHMODE 5 : Elliptical Presentation
        self.ellipticalTab = QWidget()
        self.ellipticalTab.setContentsMargins(0, 0, 0, 0)
        self.ellipticalTabLayout = QGridLayout(self.ellipticalTab)
        self.ellipticalMapFigure = plt.figure()
        self.ellipticalMapAxes = self.ellipticalMapFigure.add_subplot(111)
        self.ellipticalMapCanvas = FigureCanvas(self.ellipticalMapFigure)
        self.ellipticalMapFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.ellipticalTabLayout.addWidget(self.ellipticalMapCanvas, 0, 0, 1, 1)

        ## tabs
        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 35px; width: 200px; }")
        self.tabWidget.addTab(self.intensityTab, "Total Intensity Map")
        # self.tabWidget.addTab(self.distanceMapTab, "D-spacing Map")
        self.tabWidget.addTab(self.angularMapTab, "Angular Range Map\n(degrees)")
        self.tabWidget.addTab(self.vectorFieldTab, "Orientation and Intensity\nVector Field")
        self.tabWidget.addTab(self.ellipticalTab, "Elliptical Representation")

        # status bar
        self.statusBar = QStatusBar()
        self.imagePathLabel = QLabel('')
        self.statusLabel = QLabel('')
        self.statusBar.addWidget(QLabel('   '))
        self.statusBar.addWidget(self.imagePathLabel)
        self.progressBar = QProgressBar()
        self.progressBar.setMaximum(100)
        self.progressBar.setMinimum(0)
        self.progressBar.setHidden(True)
        # self.stopButton = QPushButton('STOP')
        # self.stopButton.clicked.connect(self.stopProcessing)
        # self.stopButton.setHidden(True)
        self.moreDetailsButton = QPushButton('More Details')
        self.moreDetailsButton.setHidden(True)
        self.rightBarLayout = QVBoxLayout()
        self.rightBarLayout.addWidget(self.statusLabel)
        self.rightBarLayout.addWidget(self.moreDetailsButton)
        self.rightBarLayout.setAlignment(Qt.AlignRight)
        self.rightBarFrame = QFrame()
        self.rightBarFrame.setLayout(self.rightBarLayout)
        self.statusBar.addPermanentWidget(self.progressBar)
        self.statusBar.addPermanentWidget(self.rightBarFrame)

        self.refreshButton = QPushButton("Refresh")
        self.refreshButton.setFixedHeight(40)

        self.mainLayout.addWidget(self.imgFrame, 0, 0, 1, 1)
        self.mainLayout.addWidget(self.refreshButton, 1, 0, 1, 1)
        self.mainLayout.addWidget(self.tabWidget, 0, 1, 2, 1)
        self.mainLayout.addWidget(self.statusBar, 2, 0, 1, 2)
        self.mainLayout.setRowStretch(0, 10)
        self.mainLayout.setRowStretch(1, 1)
        self.mainLayout.setRowStretch(2, 1)
        # self.mainLayout.setColumnStretch(0, 1)
        # self.mainLayout.setColumnStretch(1, 1)

        self.show()
        self.resize(1000, 1000)

    def setConnections(self):
        self.refreshButton.clicked.connect(self.processBatchmodeResults)

        self.img_maxInt.valueChanged.connect(self.maxIntChanged)
        self.img_minInt.valueChanged.connect(self.minIntChanged)

        self.int_maxIntMap.valueChanged.connect(self.sliderReleased)
        self.int_minIntMap.valueChanged.connect(self.sliderReleased)

        self.ds_maxIntMap.valueChanged.connect(self.sliderReleased)
        self.ds_minIntMap.valueChanged.connect(self.sliderReleased)

        self.ar_maxIntMap.valueChanged.connect(self.sliderReleased)
        self.ar_minIntMap.valueChanged.connect(self.sliderReleased)

        self.arrowLengthSlider.sliderReleased.connect(self.sliderReleased)

        self.tabWidget.currentChanged.connect(self.updateUI)
        self.moreDetailsButton.clicked.connect(self.popupImageDetails)

    def closeEvent(self, ev):
        if self.mainWin is not None:
            self.mainWin.removeWidget(self)

    def removeWidget(self, win):
        if win in self.widgetList:
            idx = self.widgetList.index(win)
            del self.widgetList[idx]

    def sliderReleased(self):
        QApplication.processEvents()
        if self.tabWidget.currentIndex() == 0:
            self.refreshAllTabs()
        elif self.tabWidget.currentIndex() == 1:
            self.update_plot['arange_maps'] = True
        elif self.tabWidget.currentIndex() == 2:
            self.updateVectorFieldArrow()

        self.updateUI()

    def plotClicked(self, event):
        self.imgFrame.setHidden(False)
        self.moreDetailsButton.setHidden(False)

        if len(self.xyIntensity) < 3:
            return

        x = self.xyIntensity[0]
        y = self.xyIntensity[1]
        x_max = len(x)

        if x[0] <= event.xdata <= x[len(x) - 1] and y[0] <= event.ydata <= y[len(y) - 1]:
            # col = 0
            # row = 0
            indexs = list(range(0, len(x)))
            col = min(indexs, key=lambda i: abs(x[i] - event.xdata))

            if (self.tabWidget.currentIndex() == 0 or self.tabWidget.currentIndex() == 1) and event.xdata < x[col]:
                col = max(col - 1, 0)

            if self.tabWidget.currentIndex() == 3:
                col = min(col + 1, len(x) - 1)

            indexs = list(range(0, len(y)))
            row = min(indexs, key=lambda i: abs(y[i] - event.ydata))
            if (self.tabWidget.currentIndex() == 0 or self.tabWidget.currentIndex() == 1) and event.ydata < y[row]:
                row = max(row - 1, 0)

            ind = row * x_max + col + self.init_number

            if ind in self.name_dict:
                img_detail = "Intensity value: " + str(self.intensity_dict[ind])
                # img_detail += "\nD-spacing: " + str(self.distance_dict[ind])
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

    def stopProcessing(self):
        self.stopProcess = True

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
        if self.batchmodeImgDetails is not None:
            ax = self.imgAxes
            ax.cla()
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

            self.imgCanvas.draw()

    def mousePressEvent(self, event):
        focused_widget = QApplication.focusWidget()
        if focused_widget != None:
            focused_widget.clearFocus()

    def updateUI(self):
        selected_tab = self.tabWidget.currentIndex()
        if selected_tab == 0:
            self.updateTotalIntenTab()
        elif selected_tab == 1:
            self.updateAngularRangeTab()
        elif selected_tab == 2:
            self.updateVectorFieldTab()
        elif selected_tab == 3:
            self.updateEllipticalTab()
        QApplication.processEvents()

    def updateTotalIntenTab(self):

        if self.update_plot['intensity_maps']:
            if len(self.xyIntensity) < 3:
                return

            x = self.xyIntensity[0]
            y = self.xyIntensity[1]
            intensity = copy.copy(self.xyIntensity[2])

            x_coor, y_coor = np.meshgrid(x, y)

            max_val = intensity.max()
            if self.int_maxIntMap.value() < 100:
                intensity[
                    intensity > self.int_maxIntMap.value() * max_val / 100.] = self.int_maxIntMap.value() * max_val / 100.
            if self.int_minIntMap.value() > 0:
                intensity[
                    intensity < self.int_minIntMap.value() * max_val / 100.] = self.int_minIntMap.value() * max_val / 100.

            ax = self.intensityMapAxes
            ax.cla()
            ax.set_title("Total Intensity Map\n")
            im = ax.pcolormesh(x_coor, y_coor, intensity, cmap=self.color_maps)
            ax.set_xlim(x.min() - self.xylim[0], x.max() + self.xylim[0])
            ax.set_ylim(y.min() - self.xylim[1], y.max() + self.xylim[1])
            # self.intensityMapFigure.colorbar(im)
            self.intensityMapFigure.tight_layout()
            self.intensityMapFigure.savefig(fullPath(self.filePath, 'cp_results/intensity_map.png'))
            self.intensityMapCanvas.draw()
            self.update_plot['intensity_maps'] = False

    def updateDspacingTab(self):

        if self.update_plot['ds_maps']:
            if len(self.xyIntensity) < 3:
                return

            x = self.xyIntensity[0]
            y = self.xyIntensity[1]
            x_max = len(x)

            if 'ds_' not in self.plots.keys():
                distances = [
                    float(self.distance_dict[i]) if i in self.distance_dict and self.distance_dict[i] != '' else 0
                    for i in
                    range(self.init_number, len(self.hdf_data) + self.init_number)]
                distances = np.array([distances[i:i + x_max] for i in range(0, len(self.hdf_data), x_max)])
                self.plots['ds_'] = copy.copy(distances)
            else:
                distances = copy.copy(self.plots['ds_'])

            # z = cv2.blur(z, (4,4))
            x_coor, y_coor = np.meshgrid(x, y)

            max_val = distances.max()
            min_val = distances.min()
            if self.ds_maxIntMap.value() < 100:
                distances[distances > self.ds_maxIntMap.value() * max_val / 100.] = max_val
            if self.ds_minIntMap.value() > 0:
                distances[distances < self.ds_minIntMap.value() * max_val / 100.] = min_val

            ax = self.distanceMapAxes
            ax.cla()
            ax.set_title("D-spacing Map\n")
            im = ax.pcolormesh(x_coor, y_coor, distances, cmap=self.color_maps)
            ax.set_xlim(x.min() - self.xylim[0], x.max() + self.xylim[0])
            ax.set_ylim(y.min() - self.xylim[1], y.max() + self.xylim[1])
            # self.distanceMapFigure.colorbar(im)
            self.distanceMapFigure.tight_layout()
            self.distanceMapFigure.savefig(fullPath(self.filePath, 'cp_results/d_spacing_map.png'))
            self.distanceMapCanvas.draw()
            self.update_plot['ds_maps'] = False

    def updateAngularRangeTab(self):

        if self.update_plot['arange_maps']:
            if len(self.xyIntensity) < 3:
                return

            x = self.xyIntensity[0]
            y = self.xyIntensity[1]
            x_max = len(x)

            if 'ar_' not in self.plots.keys():
                ang_range = [
                    convertRadtoDegrees(float(self.angrange_dict[i])) if i in self.angrange_dict and self.angrange_dict[
                                                                                                         i] != '' else 0
                    for
                    i in range(self.init_number, len(self.hdf_data) + self.init_number)]
                ang_range = [r if 0 < r <= 180 else 0 for r in ang_range]
                ang_range = np.array([ang_range[i:i + x_max] for i in range(0, len(self.hdf_data), x_max)])
                self.plots['ar_'] = copy.copy(ang_range)
            else:
                ang_range = copy.copy(self.plots['ar_'])

            # z = cv2.blur(z, (4,4))
            x_coor, y_coor = np.meshgrid(x, y)

            max_val = ang_range.max()
            min_val = ang_range.min()
            if self.ar_maxIntMap.value() < 100:
                ang_range[ang_range > self.ar_maxIntMap.value() * max_val / 100.] = max_val
            if self.ar_minIntMap.value() > 0:
                ang_range[ang_range < self.ar_minIntMap.value() * max_val / 100.] = min_val

            ax = self.angularMapAxes
            ax.cla()
            ax.set_title("Angular Range Map (Degrees)\n")
            im = ax.pcolormesh(x_coor, y_coor, ang_range, cmap=self.color_maps)
            ax.set_xlim(x.min() - self.xylim[0], x.max() + self.xylim[0])
            ax.set_ylim(y.min() - self.xylim[1], y.max() + self.xylim[1])
            # self.angularMapFigure.colorbar(im)
            self.angularMapFigure.tight_layout()
            self.angularMapFigure.savefig(fullPath(self.filePath, 'cp_results/angular_range_map.png'))
            self.angularMapCanvas.draw()
            self.update_plot['arange_maps'] = False

    def updateVectorFieldTab(self):

        if self.update_plot['vector_maps']:
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

            U = np.cos(orientation)
            V = np.sin(orientation)
            int_display = copy.copy(intensity)

            max_val = int_display.max()
            if self.int_maxIntMap.value() < 100:
                int_display[
                    int_display > self.int_maxIntMap.value() * max_val / 100.] = self.int_maxIntMap.value() * max_val / 100.
            if self.int_minIntMap.value() > 0:
                intensity[
                    int_display < self.int_minIntMap.value() * max_val / 100.] = self.int_minIntMap.value() * max_val / 100.

            speed = int_display / intensity.max()
            UN = U * speed
            VN = V * speed
            self.vec_UV = [U, V]

            ax = self.vectorFieldMapAxes
            ax.cla()
            ax.set_facecolor('black')
            ax.set_title("Orientation (direction) and Intensity (height and color) Vector Field")
            self.vec_quiver = ax.quiver(x, y, UN, VN,  # data
                                        int_display,  # colour the arrows based on this array
                                        cmap=self.color_maps,  # colour map
                                        headlength=7, headwidth=4)

            ax.set_xlim(x.min() - self.xylim[0], x.max() + self.xylim[0])
            ax.set_ylim(y.min() - self.xylim[1], y.max() + self.xylim[1])
            ax.set_aspect('auto')

            # self.vectorFieldMapFigure.colorbar(self.vec_quiver)
            self.vectorFieldMapFigure.tight_layout()
            self.vectorFieldMapFigure.savefig(fullPath(self.filePath, 'cp_results/vector_field.png'))
            self.vectorFieldMapCanvas.draw()

            if self.arrowLengthSlider.value() > 5:
                self.updateVectorFieldArrow()

            self.update_plot['vector_maps'] = False

    def updateVectorFieldArrow(self):
        if len(self.xyIntensity) < 3 or len(self.vec_UV) < 2 or self.vec_quiver is None:
            return

        intensity = self.xyIntensity[2]
        U = self.vec_UV[0]
        V = self.vec_UV[1]
        int_display = copy.copy(intensity)

        max_val = int_display.max()
        if self.int_maxIntMap.value() < 100:
            int_display[
                int_display > self.int_maxIntMap.value() * max_val / 100.] = self.int_maxIntMap.value() * max_val / 100.
        if self.int_minIntMap.value() > 0:
            int_display[
                int_display < self.int_minIntMap.value() * max_val / 100.] = self.int_minIntMap.value() * max_val / 100.

        speed = int_display / intensity.max() * (self.arrowLengthSlider.value() / 5.)
        UN = U * speed
        VN = V * speed
        self.vec_quiver.set_UVC(UN, VN)
        self.vectorFieldMapCanvas.draw_idle()
        self.vectorFieldMapFigure.savefig(fullPath(self.filePath, 'cp_results/vector_field.png'))

    def updateEllipticalTab(self):

        if self.update_plot['ellipse_maps']:

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
            if self.int_maxIntMap.value() < 100:
                int_display[
                    int_display > self.int_maxIntMap.value() * max_val / 100.] = self.int_maxIntMap.value() * max_val / 100.
            if self.int_minIntMap.value() > 0:
                int_display[
                    int_display < self.int_minIntMap.value() * max_val / 100.] = self.int_minIntMap.value() * max_val / 100.

            ax = self.ellipticalMapAxes
            ax.cla()
            ax.set_title("Elliptical Representation of Orientation (direction) and Angle Range (width)")
            patches = []
            colors = []
            for i in range(len(self.hdf_data)):

                if ranges[i] == 0:
                    e = Ellipse(xy=centers[i - self.init_number], width=self.xylim[0]/5., height=self.xylim[0]/5.)
                else:
                    e = Ellipse(xy=centers[i - self.init_number], width= self.xylim[0] * widths[i], height=self.xylim[0],
                                angle=convertRadtoDegreesEllipse(
                                    np.pi - toFloat(self.orientation_dict[i + self.init_number])))
                patches.append(e)
                # colors.append(self.intensity_dict[i + self.init_number])
                if i < len(int_display):
                    colors.append(int_display[i])
                else:
                    colors.append(0)

            p = PatchCollection(patches, cmap=self.color_maps)
            p.set_array(np.array(colors))
            ax.add_collection(p)
            ax.set_facecolor('black')
            ax.set_xlim(x.min() - self.xylim[0], x.max() + self.xylim[0])
            ax.set_ylim(y.min() - self.xylim[1], y.max() + self.xylim[1])
            ax.set_aspect('auto')

            # self.ellipticalMapFigure.colorbar(p)
            self.ellipticalMapFigure.tight_layout()
            self.ellipticalMapFigure.savefig(fullPath(self.filePath, 'cp_results/direction_width.png'))
            self.ellipticalMapCanvas.draw()
            self.update_plot['ellipse_maps'] = False

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
                all_rings = all_rings.sort_values(['angle fitting error'], ascending=True)
                best_ring = all_rings.iloc[0]
                good_model = float(best_ring['angle fitting error']) < 1. and best_ring['angle sigma'] < 1.
                self.orientation_dict[index] = best_ring['angle'] if pd.notnull(
                    best_ring['angle']) and good_model else 0
                self.angrange_dict[index] = best_ring['angle sigma'] if pd.notnull(
                    best_ring['angle sigma']) and good_model else 0
                self.distance_dict[index] = 0
            else:
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

        nRows = len(self.hdf_data) / nCols
        all_xs = np.reshape(np.array([v[0] for k, v in self.coord_dict.items()]), (nRows, nCols))
        all_ys = np.reshape(np.array([v[1] for k, v in self.coord_dict.items()]), (nRows, nCols))

        x = np.mean(all_xs, axis=0)
        y = np.mean(all_ys, axis=1)

        # Check if any error on coordinates
        x_grad = abs(x[1] - x[0])
        y_grad = abs(y[1] - y[0])

        # Plot heatmap for intensity
        z = [float(self.intensity_dict[i]) if i in self.intensity_dict else 0 for i in
             range(self.init_number, len(self.hdf_data) + self.init_number)]
        # z = np.array([z[i:i + x_max] for i in range(0, , x_max)])
        # z = cv2.blur(z, (4,4))
        # intensity = np.array(z)
        intensity = np.reshape(z, (len(y), len(x)))

        self.xyIntensity = [x, y, intensity]
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
