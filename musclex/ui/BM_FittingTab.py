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
from pyqt_utils import *
import matplotlib.patches as patches
from ..modules.BioImage import getCardiacGraph
from ..utils.image_processor import *

class BM_FittingTab(QWidget):
    """
    Fitting Tabs : left or right
    Display fitting graph and providing options
    """
    def __init__(self, parent, side):
        QWidget.__init__(self)
        self.parent = parent
        self.side = side
        self.syncUI = False
        self.initUI()
        self.setAllToolTips()
        self.setConnections()

    def initUI(self):
        """
        Initial all GUIs including : 4 plots and result table
        """
        self.setContentsMargins(0, 0, 0, 0)
        self.fittingTabLayout = QVBoxLayout(self)

        self.fitSettingsGrp = QGroupBox("Settings")
        self.fitSettingLayout = QGridLayout(self.fitSettingsGrp)
        self.sigmaCSpinBx = QDoubleSpinBox()
        self.sigmaCSpinBx.setMinimum(-100)
        self.sigmaCSpinBx.setDecimals(6)
        self.sigmaCSpinBx.setMaximum(100)
        self.sigmaCSpinBx.setKeyboardTracking(False)
        self.sigmaCSpinBx.setValue(1.)

        self.fixSigmaD = QCheckBox("Fixed Sigma D :")
        self.sigmaDSpinBx = QDoubleSpinBox()
        self.sigmaDSpinBx.setEnabled(False)
        self.sigmaDSpinBx.setMinimum(-100)
        self.sigmaDSpinBx.setMaximum(100)
        self.sigmaDSpinBx.setDecimals(6)
        self.sigmaDSpinBx.setKeyboardTracking(False)
        self.sigmaDSpinBx.setValue(1.)
        self.fixSigmaS = QCheckBox("Fixed Sigma S :")
        self.sigmaSSpinBx = QDoubleSpinBox()
        self.sigmaSSpinBx.setEnabled(False)
        self.sigmaSSpinBx.setMinimum(-100)
        self.sigmaSSpinBx.setMaximum(100)
        self.sigmaSSpinBx.setDecimals(6)
        self.sigmaSSpinBx.setKeyboardTracking(False)
        self.sigmaSSpinBx.setValue(0.0001)
        self.fixGamma = QCheckBox("Fixed gamma :")
        self.gammaSpinBx = QDoubleSpinBox()
        self.gammaSpinBx.setEnabled(False)
        self.gammaSpinBx.setMinimum(-100)
        self.gammaSpinBx.setMaximum(100)
        self.gammaSpinBx.setDecimals(6)
        self.gammaSpinBx.setKeyboardTracking(False)
        self.gammaSpinBx.setValue(1)
        self.refittingB = QPushButton("Re-fitting")

        self.fitSettingLayout.addWidget(QLabel("Sigma C :"), 0, 0, 1, 1)
        self.fitSettingLayout.addWidget(self.sigmaCSpinBx, 0, 1, 1, 1)
        self.fitSettingLayout.addWidget(self.fixSigmaD, 1, 0, 1, 1)
        self.fitSettingLayout.addWidget(self.sigmaDSpinBx, 1, 1, 1, 1)
        self.fitSettingLayout.addWidget(self.fixSigmaS, 2, 0, 1, 1)
        self.fitSettingLayout.addWidget(self.sigmaSSpinBx, 2, 1, 1, 1)
        self.fitSettingLayout.addWidget(self.fixGamma, 3, 0, 1, 1)
        self.fitSettingLayout.addWidget(self.gammaSpinBx, 3, 1, 1, 1)
        self.fitSettingLayout.setRowMinimumHeight(0, 10)

        self.skeletalGrp = QGroupBox("Skeletal Muscle (Z line)")
        self.skeletalGrp.setEnabled(False)
        self.skeletalLayout = QGridLayout(self.skeletalGrp)
        self.fixedZline = QCheckBox("Fixed Center : ")
        self.zlineSpnBx = QDoubleSpinBox()
        self.zlineSpnBx.setDecimals(0)
        self.zlineSpnBx.setRange(0, 500)
        self.zlineSpnBx.setKeyboardTracking(False)
        self.fixedIntZ = QCheckBox("Fixed Intensity : ")
        self.intZSpnBx = QDoubleSpinBox()
        self.intZSpnBx.setDecimals(3)
        self.intZSpnBx.setRange(0, 10000000)
        self.intZSpnBx.setKeyboardTracking(False)
        self.fixedSigZ = QCheckBox("Fixed Sigma : ")
        self.sigZSpnBx = QDoubleSpinBox()
        self.sigZSpnBx.setDecimals(6)
        self.sigZSpnBx.setRange(-100, 100)
        self.sigZSpnBx.setKeyboardTracking(False)
        self.fixedGammaZ = QCheckBox("Fixed Gamma : ")
        self.gammaZSpnBx = QDoubleSpinBox()
        self.gammaZSpnBx.setDecimals(6)
        self.gammaZSpnBx.setRange(-100, 100)
        self.gammaZSpnBx.setKeyboardTracking(False)

        self.skeletalLayout.addWidget(self.fixedZline, 0, 0, 1, 1)
        self.skeletalLayout.addWidget(self.zlineSpnBx, 0, 1, 1, 1)
        self.skeletalLayout.addWidget(self.fixedIntZ, 1, 0, 1, 1)
        self.skeletalLayout.addWidget(self.intZSpnBx, 1, 1, 1, 1)
        self.skeletalLayout.addWidget(self.fixedSigZ, 2, 0, 1, 1)
        self.skeletalLayout.addWidget(self.sigZSpnBx, 2, 1, 1, 1)
        self.skeletalLayout.addWidget(self.fixedGammaZ, 3, 0, 1, 1)
        self.skeletalLayout.addWidget(self.gammaZSpnBx, 3, 1, 1, 1)

        self.fittingTabLayout.addSpacing(10)
        self.fittingTabLayout.addWidget(self.fitSettingsGrp)
        self.fittingTabLayout.addSpacing(10)
        self.fittingTabLayout.addWidget(self.skeletalGrp)
        self.fittingTabLayout.addWidget(self.refittingB)


    def setAllToolTips(self):
        """
        Set Tooltips for widgets
        """
        self.skeletalGrp.setToolTip("Fit model with the skeletal peaks")
        self.sigmaCSpinBx.setToolTip("Select the constant sigma C for fitting model")
        self.refittingB.setToolTip("Refit the model again with current settings")

    def setConnections(self):
        """
        Set connection for interactive widgets
        """
        self.sigmaCSpinBx.valueChanged.connect(self.fixedFittingParams)
        self.sigmaDSpinBx.valueChanged.connect(self.fixedFittingParams)
        self.sigmaSSpinBx.valueChanged.connect(self.fixedFittingParams)
        self.fixSigmaD.stateChanged.connect(self.fixedParamChecked)
        self.fixSigmaS.stateChanged.connect(self.fixedParamChecked)
        self.fixGamma.stateChanged.connect(self.fixedParamChecked)
        self.gammaSpinBx.valueChanged.connect(self.fixedFittingParams)
        self.refittingB.clicked.connect(self.fixedFittingParams)

        self.fixedIntZ.stateChanged.connect(self.skeletalChecked)
        self.fixedZline.stateChanged.connect(self.skeletalChecked)
        self.fixedSigZ.stateChanged.connect(self.skeletalChecked)
        self.fixedGammaZ.stateChanged.connect(self.skeletalChecked)
        self.sigZSpnBx.valueChanged.connect(self.skeletalChanged)
        self.intZSpnBx.valueChanged.connect(self.skeletalChanged)
        self.zlineSpnBx.valueChanged.connect(self.skeletalChanged)
        self.gammaZSpnBx.valueChanged.connect(self.skeletalChanged)


    def syncSpinBoxes(self, info):
        self.syncUI = True
        side = self.side
        self.fixSigmaD.setChecked(info.has_key(side+'_fix_sigmad'))
        self.fixSigmaS.setChecked(info.has_key(side+'_fix_sigmas'))
        self.sigmaDSpinBx.setEnabled(info.has_key(side+'_fix_sigmad'))
        self.sigmaSSpinBx.setEnabled(info.has_key(side+'_fix_sigmas'))

        if info.has_key('fit_results'):
            fit_result = info['fit_results']
            self.sigmaDSpinBx.setValue(fit_result[side+'_sigmad'])
            self.sigmaSSpinBx.setValue(fit_result[side+'_sigmas'])
            self.skeletalGrp.setEnabled(fit_result['isSkeletal'])
            self.sigmaCSpinBx.setValue(fit_result[side+'_sigmac'])
            self.gammaSpinBx.setValue(fit_result[side+'_gamma'])

            if fit_result['isSkeletal']:
                self.zlineSpnBx.setValue(fit_result[side+'_zline'])
                self.sigZSpnBx.setValue(fit_result[side+'_sigmaz'])
                self.intZSpnBx.setValue(fit_result[side+'_intz'])
                self.gammaZSpnBx.setValue(fit_result[side+'_gammaz'])

        self.syncUI = False

    def initSpinBoxes(self, info):
        self.syncUI = True
        side = self.side
        if info.has_key('fit_results'):
            fit_result = info['fit_results']
            self.sigmaDSpinBx.setValue(fit_result[side+'_sigmad'])
            self.sigmaSSpinBx.setValue(fit_result[side+'_sigmas'])
            self.gammaSpinBx.setValue(fit_result[side+'_gamma'])
            # self.nPeakSpnBx.setValue(len(fit_result['areas']))
            # self.modelSelect.setCurrentIndex(self.modelSelect.findText(fit_result["model"]))
            self.sigmaCSpinBx.setValue(fit_result[side+'_sigmac'])
            self.skeletalGrp.setEnabled(fit_result['isSkeletal'])
            if fit_result['isSkeletal']:
                self.zlineSpnBx.setValue(fit_result[side+'_zline'])
                self.sigZSpnBx.setValue(fit_result[side+'_sigmaz'])
                self.intZSpnBx.setValue(fit_result[side+'_intz'])

            self.gammaSpinBx.setHidden(fit_result['model'] != 'Voigt')
            self.fixGamma.setHidden(fit_result['model'] != 'Voigt')
            self.fixedGammaZ.setHidden(fit_result['model'] != 'Voigt')
            self.gammaZSpnBx.setHidden(fit_result['model'] != 'Voigt')

        self.fixSigmaD.setChecked(info.has_key(side+'_fix_sigmad'))
        self.fixSigmaS.setChecked(info.has_key(side+'_fix_sigmas'))
        self.sigmaDSpinBx.setEnabled(info.has_key(side+'_fix_sigmad'))
        self.sigmaSSpinBx.setEnabled(info.has_key(side+'_fix_sigmas'))

        self.fixGamma.setChecked(info.has_key(side+'_fix_gamma'))
        self.gammaSpinBx.setEnabled(info.has_key(side+'_fix_gamma'))

        self.fixedZline.setChecked(info.has_key(side+'_fix_zline'))
        self.fixedZline.setChecked(info.has_key(side+'_fix_zline'))
        self.fixedSigZ.setChecked(info.has_key(side+'_fix_sigz'))
        self.fixedIntZ.setChecked(info.has_key(side+'_fix_intz'))
        self.fixedGammaZ.setChecked(info.has_key(side+'_fix_gammaz'))
        self.zlineSpnBx.setEnabled(info.has_key(side+'_fix_zline'))
        self.sigZSpnBx.setEnabled(info.has_key(side+'_fix_sigz'))
        self.intZSpnBx.setEnabled(info.has_key(side+'_fix_intz'))
        self.gammaZSpnBx.setEnabled(info.has_key(side+'_fix_gammaz'))
        self.syncUI = False

    def fixedParamChecked(self):
        """
        Enable/Disable spinboxes
        """
        bioImg = self.parent.bioImg
        if self.syncUI or bioImg is None:
            return

        self.sigmaDSpinBx.setEnabled(self.fixSigmaD.isChecked())
        self.sigmaSSpinBx.setEnabled(self.fixSigmaS.isChecked())
        self.gammaSpinBx.setEnabled(self.fixGamma.isChecked())

    def fixedFittingParams(self):
        """
        Fixed Value Changed. Remove fit_results from info dict to make it be re-calculated
        """
        self.parent.refreshAllFittingParams()

    def skeletalChecked(self):
        """
        Enable/Disable spinboxes
        """
        if self.parent.bioImg is None or self.syncUI:
            return

        self.zlineSpnBx.setEnabled(self.fixedZline.isChecked())
        self.sigZSpnBx.setEnabled(self.fixedSigZ.isChecked())
        self.intZSpnBx.setEnabled(self.fixedIntZ.isChecked())
        self.gammaZSpnBx.setEnabled(self.fixedGammaZ.isChecked())

    def skeletalChanged(self):
        """
        Reset all about z line and re-process image
        """
        bioImg = self.parent.bioImg
        if bioImg is None or self.syncUI:
            return
        self.parent.refreshFittingParams(self.side)
        self.parent.processImage()

    def hideGamma(self, flag):
        """
        Hide gamma settings if model is not Voigt
        """
        self.gammaSpinBx.setHidden(flag)
        self.fixGamma.setHidden(flag)
        self.gammaZSpnBx.setHidden(flag)
        self.fixedGammaZ.setHidden(flag)

    def getFittingSettings(self):
        """
        Get All settings that are necessary for bioImage to process
        :return:
        """
        settings = {}
        side = self.side
        settings[side+'_sigmac'] = self.sigmaCSpinBx.value()

        # get all locked parameters
        if self.fixSigmaD.isChecked():
            settings[side+'_fix_sigmad'] = self.sigmaDSpinBx.value()

        if self.fixSigmaS.isChecked():
            settings[side+'_fix_sigmas'] = self.sigmaSSpinBx.value()

        if self.fixGamma.isChecked():
            settings[side+'_fix_gamma'] = self.gammaSpinBx.value()

        if self.parent.skeletalChkBx.isChecked():
            if self.fixedIntZ.isChecked():
                settings[side+'_fix_intz'] = self.intZSpnBx.value()
            if self.fixedSigZ.isChecked():
                settings[side+'_fix_sigz'] = self.sigZSpnBx.value()
            if self.fixedZline.isChecked():
                settings[side+'_fix_zline'] = self.zlineSpnBx.value()
            if self.fixedGammaZ.isChecked():
                settings[side + '_fix_gammaz'] = self.gammaZSpnBx.value()

        return settings
