from .pyqt_utils import *


class UnalignedImagesDialog(QDialog):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Detection Settings")
        self.initUI()
        #self.setConnections()
    def initUI(self):
        self.mainLayout = QVBoxLayout()
        self.setLayout(self.mainLayout)

        self.distanceModeGrp = QGroupBox()
        self.distanceModeLayout = QHBoxLayout()
        self.distanceModeGrp.setLayout(self.distanceModeLayout)
        self.distanceMode = QComboBox()
        self.distanceMode.addItems(['Image', 'Center', 'Center + Angle', 'Center + Image', 'Center + Angle + Image'])
        self.distanceLayoutLabel = QLabel("Distance Mode")

        self.distanceModeLayout.addWidget(self.distanceLayoutLabel)
        self.distanceModeLayout.addWidget(self.distanceMode)

        self.mainLayout.addWidget(self.distanceModeGrp)

        self.coefficientGroupBox = QGroupBox("Coefficients")
        self.coefficientGroupBoxLayout = QGridLayout()
        self.coefficientGroupBox.setLayout(self.coefficientGroupBoxLayout)

        self.imageCoeffcientLabel = QLabel("Image Coefficient")
        self.centerCoefficientLabel = QLabel("Center Coefficient")
        self.angleCoefficientLabel = QLabel("Angle Coefficient")


        self.imageCoefficient = QDoubleSpinBox()
        self.centerCoefficient = QDoubleSpinBox()
        self.angleCoefficient = QDoubleSpinBox()
        self.imageCoefficient.setValue(0.33)
        self.centerCoefficient.setValue(0.33)
        self.angleCoefficient.setValue(0.33)
        self.imageCoefficient.setDecimals(2)
        self.centerCoefficient.setDecimals(2)
        self.angleCoefficient.setDecimals(2)
        self.imageCoefficient.setSingleStep(0.1)
        self.centerCoefficient.setSingleStep(0.1)
        self.angleCoefficient.setSingleStep(0.1)
        self.imageCoefficient.setMaximum(1)
        self.centerCoefficient.setMaximum(1)
        self.angleCoefficient.setMaximum(1)
        self.imageCoefficient.setMinimum(0)
        self.centerCoefficient.setMinimum(0)
        self.angleCoefficient.setMinimum(0)

        self.coefficientGroupBoxLayout.addWidget(self.imageCoeffcientLabel, 0, 0, 1, 2)
        self.coefficientGroupBoxLayout.addWidget(self.imageCoefficient, 0, 2, 1, 2)
        self.coefficientGroupBoxLayout.addWidget(self.centerCoefficientLabel, 1, 0, 1, 2)
        self.coefficientGroupBoxLayout.addWidget(self.centerCoefficient, 1, 2, 1, 2)
        self.coefficientGroupBoxLayout.addWidget(self.angleCoefficientLabel, 2, 0, 1, 2)    
        self.coefficientGroupBoxLayout.addWidget(self.angleCoefficient, 2, 2, 1, 2)

        self.mainLayout.addWidget(self.coefficientGroupBox)

        self.exitButton = QPushButton("OK")
        self.exitButton.clicked.connect(self.onExitPressed)
        self.mainLayout.addWidget(self.exitButton)

    def onExitPressed(self):
        # image, center, angle = self.imageCoefficient.value(), self.centerCoefficient.value(), self.angleCoefficient.value()
        # if (image + center + angle != 1):
        #     QMessageBox.warning(self, "Warning", "The sum of the coefficients should be 1.")
        #     return
        self.image = self.imageCoefficient.value()
        self.center = self.centerCoefficient.value()
        self.angle = self.angleCoefficient.value()
        self.distance_mode = self.distanceMode.currentIndex() + 1
        self.accept()