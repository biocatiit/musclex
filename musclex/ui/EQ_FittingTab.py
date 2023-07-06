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

from .pyqt_utils import *
from ..utils.image_processor import *

class EQ_FittingTab(QWidget):
    """
    Fitting Tabs : left or right
    Display fitting graph and providing options
    """
    def __init__(self, parent, side):
        QWidget.__init__(self)
        self.parent = parent
        self.side = side
        self.syncUI = False
        self.editableVars = {}
        self.initUI()
        self.setAllToolTips()
        self.setConnections()

    def initUI(self):
        """
        Initial all GUIs including : 4 plots and result table
        """
        self.setContentsMargins(0, 0, 0, 0)
        self.fittingTabLayout = QGridLayout(self)

        self.fitSettingsGrp = QGroupBox("Settings")
        self.fitSettingLayout = QGridLayout(self.fitSettingsGrp)

        self.fixSigmaC = QCheckBox("Fixed Sigma C :")
        # self.fixSigmaC.setChecked(True)
        self.sigmaCSpinBx = QDoubleSpinBox()
        self.sigmaCSpinBx.setEnabled(False)
        self.sigmaCSpinBx.setMinimum(-100)
        self.sigmaCSpinBx.setDecimals(6)
        self.sigmaCSpinBx.setMaximum(100)
        self.sigmaCSpinBx.setKeyboardTracking(False)
        self.sigmaCSpinBx.setValue(1.)
        self.sigmaCSpinBx.setObjectName('sigmaCSpinBx')
        self.editableVars[self.sigmaCSpinBx.objectName()] = None

        self.fixSigmaD = QCheckBox("Fixed Sigma D :")
        self.sigmaDSpinBx = QDoubleSpinBox()
        self.sigmaDSpinBx.setEnabled(False)
        self.sigmaDSpinBx.setMinimum(-100)
        self.sigmaDSpinBx.setMaximum(100)
        self.sigmaDSpinBx.setDecimals(6)
        self.sigmaDSpinBx.setKeyboardTracking(False)
        self.sigmaDSpinBx.setValue(1.)
        self.sigmaDSpinBx.setObjectName('sigmaDSpinBx')
        self.editableVars[self.sigmaDSpinBx.objectName()] = None

        self.fixSigmaS = QCheckBox("Fixed Sigma S :")
        # self.fixSigmaS.setChecked(True)
        self.sigmaSSpinBx = QDoubleSpinBox()
        self.sigmaSSpinBx.setEnabled(False)
        self.sigmaSSpinBx.setMinimum(-100)
        self.sigmaSSpinBx.setMaximum(100)
        self.sigmaSSpinBx.setDecimals(6)
        self.sigmaSSpinBx.setKeyboardTracking(False)
        self.sigmaSSpinBx.setValue(0.0001)
        self.sigmaSSpinBx.setObjectName('sigmaSSpinBx')
        self.editableVars[self.sigmaSSpinBx.objectName()] = None

        self.fixGamma = QCheckBox("Fixed gamma :")
        self.gammaSpinBx = QDoubleSpinBox()
        self.gammaSpinBx.setEnabled(False)
        self.gammaSpinBx.setMinimum(-100)
        self.gammaSpinBx.setMaximum(100)
        self.gammaSpinBx.setDecimals(6)
        self.gammaSpinBx.setKeyboardTracking(False)
        self.gammaSpinBx.setValue(1)
        self.gammaSpinBx.setObjectName('gammaSpinBx')
        self.editableVars[self.gammaSpinBx.objectName()] = None

        self.fitSettingLayout.addWidget(self.fixSigmaC, 0, 0, 1, 1)
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
        self.zlineSpnBx.setObjectName('zlineSpnBx')
        self.zlineSpnBx.setEnabled(False)
        self.editableVars[self.zlineSpnBx.objectName()] = None
        self.zlineSpnBx.setDecimals(0)
        self.zlineSpnBx.setRange(0, 500)
        self.zlineSpnBx.setKeyboardTracking(False)
        self.fixedIntZ = QCheckBox("Fixed Intensity : ")
        self.intZSpnBx = QDoubleSpinBox()
        self.intZSpnBx.setObjectName('intZSpnBx')
        self.intZSpnBx.setEnabled(False)
        self.editableVars[self.intZSpnBx.objectName()] = None
        self.intZSpnBx.setDecimals(3)
        self.intZSpnBx.setRange(0, 10000000)
        self.intZSpnBx.setKeyboardTracking(False)
        self.fixedSigZ = QCheckBox("Fixed Sigma : ")
        self.sigZSpnBx = QDoubleSpinBox()
        self.sigZSpnBx.setObjectName('sigZSpnBx')
        self.sigZSpnBx.setEnabled(False)
        self.editableVars[self.sigZSpnBx.objectName()] = None
        self.sigZSpnBx.setDecimals(6)
        self.sigZSpnBx.setRange(-100, 100)
        self.sigZSpnBx.setKeyboardTracking(False)
        self.fixedGammaZ = QCheckBox("Fixed Gamma : ")
        self.gammaZSpnBx = QDoubleSpinBox()
        self.gammaZSpnBx.setObjectName('gammaZSpnBx')
        self.editableVars[self.gammaZSpnBx.objectName()] = None
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

        self.extraPeakGrp = QGroupBox("Extra Peak")
        self.extraPeakGrp.setEnabled(False)
        self.extraPeakLayout = QGridLayout(self.extraPeakGrp)
        self.fixedZlineEP = QCheckBox("Fixed Center : ")
        self.zlineSpnBxEP = QDoubleSpinBox()
        self.zlineSpnBxEP.setObjectName('zlineSpnBx')
        self.zlineSpnBxEP.setEnabled(False)
        self.editableVars[self.zlineSpnBxEP.objectName()] = None
        self.zlineSpnBxEP.setDecimals(0)
        self.zlineSpnBxEP.setRange(0, 500)
        self.zlineSpnBxEP.setKeyboardTracking(False)
        self.fixedIntZEP = QCheckBox("Fixed Intensity : ")
        self.intZSpnBxEP = QDoubleSpinBox()
        self.intZSpnBxEP.setObjectName('intZSpnBx')
        self.intZSpnBxEP.setEnabled(False)
        self.editableVars[self.intZSpnBxEP.objectName()] = None
        self.intZSpnBxEP.setDecimals(3)
        self.intZSpnBxEP.setRange(0, 10000000)
        self.intZSpnBxEP.setKeyboardTracking(False)
        self.fixedSigZEP = QCheckBox("Fixed Sigma : ")
        self.sigZSpnBxEP = QDoubleSpinBox()
        self.sigZSpnBxEP.setObjectName('sigZSpnBx')
        self.sigZSpnBxEP.setEnabled(False)
        self.editableVars[self.sigZSpnBxEP.objectName()] = None
        self.sigZSpnBxEP.setDecimals(6)
        self.sigZSpnBxEP.setRange(-100, 100)
        self.sigZSpnBxEP.setKeyboardTracking(False)
        self.fixedGammaZEP = QCheckBox("Fixed Gamma : ")
        self.gammaZSpnBxEP = QDoubleSpinBox()
        self.gammaZSpnBxEP.setObjectName('gammaZSpnBx')
        self.editableVars[self.gammaZSpnBxEP.objectName()] = None
        self.gammaZSpnBxEP.setDecimals(6)
        self.gammaZSpnBxEP.setRange(-100, 100)
        self.gammaZSpnBxEP.setKeyboardTracking(False)

        self.extraPeakLayout.addWidget(self.fixedZlineEP, 0, 0, 1, 1)
        self.extraPeakLayout.addWidget(self.zlineSpnBxEP, 0, 1, 1, 1)
        self.extraPeakLayout.addWidget(self.fixedIntZEP, 1, 0, 1, 1)
        self.extraPeakLayout.addWidget(self.intZSpnBxEP, 1, 1, 1, 1)
        self.extraPeakLayout.addWidget(self.fixedSigZEP, 2, 0, 1, 1)
        self.extraPeakLayout.addWidget(self.sigZSpnBxEP, 2, 1, 1, 1)
        self.extraPeakLayout.addWidget(self.fixedGammaZEP, 3, 0, 1, 1)
        self.extraPeakLayout.addWidget(self.gammaZSpnBxEP, 3, 1, 1, 1)

        # self.fittingTabLayout.addSpacing(10)
        self.fittingTabLayout.addWidget(self.fitSettingsGrp, 0, 0)
        # self.fittingTabLayout.addSpacing()
        self.fittingTabLayout.addWidget(self.skeletalGrp, 1, 0)
        # self.fittingTabLayout.addSpacing()
        self.fittingTabLayout.addWidget(self.extraPeakGrp, 2, 0)

    def setAllToolTips(self):
        """
        Set Tooltips for widgets
        """
        self.skeletalGrp.setToolTip("Fit model with the skeletal peaks")
        self.sigmaCSpinBx.setToolTip("Select the constant sigma C for fitting model")

    def setConnections(self):
        """
        Set connection for interactive widgets
        """
        self.sigmaCSpinBx.editingFinished.connect(lambda: self.fixedFittingParams('sigmaC', self.sigmaCSpinBx))
        self.sigmaDSpinBx.editingFinished.connect(lambda: self.fixedFittingParams('sigmaD', self.sigmaDSpinBx))
        self.sigmaSSpinBx.editingFinished.connect(lambda: self.fixedFittingParams('sigmaS', self.sigmaSSpinBx))
        self.fixSigmaC.stateChanged.connect(self.fixSigmaCChecked)
        self.fixSigmaD.stateChanged.connect(self.fixedParamChecked)
        self.fixSigmaS.stateChanged.connect(self.fixSigmaSChecked)
        self.fixGamma.stateChanged.connect(self.fixedParamChecked)
        self.gammaSpinBx.editingFinished.connect(lambda: self.fixedFittingParams('gamma', self.gammaSpinBx))

        self.fixedIntZ.stateChanged.connect(self.skeletalChecked)
        self.fixedZline.stateChanged.connect(self.skeletalChecked)
        self.fixedSigZ.stateChanged.connect(self.skeletalChecked)
        self.fixedGammaZ.stateChanged.connect(self.skeletalChecked)
        self.sigZSpnBx.editingFinished.connect(lambda: self.skeletalChanged('sigZ', self.sigZSpnBx))
        self.intZSpnBx.editingFinished.connect(lambda: self.skeletalChanged('intZ', self.intZSpnBx))
        self.zlineSpnBx.editingFinished.connect(lambda: self.skeletalChanged('zline', self.zlineSpnBx))
        self.gammaZSpnBx.editingFinished.connect(lambda: self.skeletalChanged('gammaZ', self.gammaZSpnBx))

        self.fixedIntZEP.stateChanged.connect(self.skeletalChecked)
        self.fixedZlineEP.stateChanged.connect(self.skeletalChecked)
        self.fixedSigZEP.stateChanged.connect(self.skeletalChecked)
        self.fixedGammaZEP.stateChanged.connect(self.skeletalChecked)
        self.sigZSpnBxEP.editingFinished.connect(lambda: self.skeletalChanged('sigZ_EP', self.sigZSpnBxEP))
        self.intZSpnBxEP.editingFinished.connect(lambda: self.skeletalChanged('intZ_EP', self.intZSpnBxEP))
        self.zlineSpnBxEP.editingFinished.connect(lambda: self.skeletalChanged('zline_EP', self.zlineSpnBxEP))
        self.gammaZSpnBxEP.editingFinished.connect(lambda: self.skeletalChanged('gammaZ_EP', self.gammaZSpnBxEP))

    def syncSpinBoxes(self, info):
        """
        Synchronize spin boxes
        """
        self.syncUI = True
        side = self.side
        self.fixSigmaC.setChecked(side+'_fix_sigmac' in info)
        self.fixSigmaD.setChecked(side+'_fix_sigmad' in info)
        self.fixSigmaS.setChecked(side+'_fix_sigmas' in info)
        self.sigmaDSpinBx.setEnabled(side+'_fix_sigmad' in info)
        self.sigmaSSpinBx.setEnabled(side+'_fix_sigmas' in info)

        if 'fit_results' in info:
            fit_result = info['fit_results']
            self.sigmaCSpinBx.setValue(fit_result[side+'_sigmac'])
            self.sigmaDSpinBx.setValue(fit_result[side+'_sigmad'])
            self.sigmaSSpinBx.setValue(fit_result[side+'_sigmas'])
            self.skeletalGrp.setEnabled(fit_result['isSkeletal'])
            self.extraPeakGrp.setEnabled(fit_result['isExtraPeak'])
            self.sigmaCSpinBx.setValue(fit_result[side+'_sigmac'])
            self.gammaSpinBx.setValue(fit_result[side+'_gamma'])

            if fit_result['isSkeletal']:
                self.zlineSpnBx.setValue(fit_result[side+'_zline'])
                self.sigZSpnBx.setValue(fit_result[side+'_sigmaz'])
                self.intZSpnBx.setValue(fit_result[side+'_intz'])
                self.gammaZSpnBx.setValue(fit_result[side+'_gammaz'])
                if fit_result['isExtraPeak']:
                    self.zlineSpnBxEP.setValue(fit_result[side+'_zline_EP'])
                    self.sigZSpnBxEP.setValue(fit_result[side+'_sigmaz_EP'])
                    self.intZSpnBxEP.setValue(fit_result[side+'_intz_EP'])
                    self.gammaZSpnBxEP.setValue(fit_result[side+'_gammaz_EP'])

        self.syncUI = False

    def initSpinBoxes(self, info):
        """
        Initialize spin box
        """
        self.syncUI = True
        side = self.side
        if 'fit_results' in info:
            fit_result = info['fit_results']
            self.sigmaCSpinBx.setValue(fit_result[side+'_sigmac'])
            if side+'_fix_sigmac' in info:
                self.fixSigmaC.setChecked(True)
                self.sigmaCSpinBx.setEnabled(True)
            else:
                self.fixSigmaC.setChecked(False)
                self.sigmaCSpinBx.setEnabled(False)
            self.sigmaDSpinBx.setValue(fit_result[side+'_sigmad'])
            if side+'_fix_sigmad' in info:
                self.fixSigmaD.setChecked(True)
                self.sigmaDSpinBx.setEnabled(True)
            else:
                self.fixSigmaD.setChecked(False)
                self.sigmaDSpinBx.setEnabled(False)
            self.sigmaSSpinBx.setValue(fit_result[side+'_sigmas'])
            if side+'_fix_sigmas' in info:
                self.fixSigmaS.setChecked(True)
                self.sigmaSSpinBx.setEnabled(True)
            else:
                self.fixSigmaS.setChecked(False)
                self.sigmaSSpinBx.setEnabled(False)
            self.gammaSpinBx.setValue(fit_result[side+'_gamma'])
            # self.nPeakSpnBx.setValue(len(fit_result['areas']))
            # self.modelSelect.setCurrentIndex(self.modelSelect.findText(fit_result["model"]))
            self.skeletalGrp.setEnabled(fit_result['isSkeletal'])
            self.extraPeakGrp.setEnabled(fit_result['isExtraPeak'])
            if fit_result['isSkeletal']:
                self.zlineSpnBx.setValue(fit_result[side+'_zline'])
                self.sigZSpnBx.setValue(fit_result[side+'_sigmaz'])
                self.intZSpnBx.setValue(fit_result[side+'_intz'])
                if fit_result['isExtraPeak']:
                    self.zlineSpnBxEP.setValue(fit_result[side+'_zline_EP'])
                    self.sigZSpnBxEP.setValue(fit_result[side+'_sigmaz_EP'])
                    self.intZSpnBxEP.setValue(fit_result[side+'_intz_EP'])

            self.gammaSpinBx.setEnabled(self.fixGamma.isChecked())
            self.gammaSpinBx.setHidden(fit_result['model'] != 'Voigt')
            self.fixGamma.setHidden(fit_result['model'] != 'Voigt')
            self.fixedGammaZ.setHidden(fit_result['model'] != 'Voigt')
            self.gammaZSpnBx.setEnabled(self.fixedGammaZ.isChecked())
            self.gammaZSpnBx.setHidden(fit_result['model'] != 'Voigt')
            self.fixedGammaZEP.setHidden(fit_result['model'] != 'Voigt')
            self.gammaZSpnBxEP.setEnabled(self.fixedGammaZEP.isChecked())
            self.gammaZSpnBxEP.setHidden(fit_result['model'] != 'Voigt')

        if side+'_fix_sigmac' in info:
            self.fixSigmaC.setChecked(True)
            self.sigmaCSpinBx.setEnabled(True)
            self.sigmaCSpinBx.setValue(info[side + '_fix_sigmac'])

        if side+'_fix_sigmad' in info:
            self.fixSigmaD.setChecked(True)
            self.sigmaDSpinBx.setEnabled(True)
            self.sigmaDSpinBx.setValue(info[side + '_fix_sigmad'])

        if side+'_fix_sigmas' in info:
            self.fixSigmaS.setChecked(True)
            self.sigmaSSpinBx.setEnabled(True)
            self.sigmaSSpinBx.setValue(info[side+'_fix_sigmas'])

        if side+'_fix_gamma' in info:
            self.fixGamma.setChecked(True)
            self.gammaSpinBx.setEnabled(True)
            self.gammaSpinBx.setValue(info[side+'_fix_gamma'])

        if side+'_fix_zline' in info:
            self.fixedZline.setChecked(True)
            self.zlineSpnBx.setEnabled(True)
            self.zlineSpnBx.setValue(info[side + '_fix_zline'])

        if side+'_fix_intz' in info:
            self.fixedIntZ.setChecked(True)
            self.intZSpnBx.setEnabled(True)
            self.intZSpnBx.setValue(info[side + '_fix_intz'])

        if side+'_fix_gammaz' in info:
            self.fixedGammaZ.setChecked(True)
            self.gammaZSpnBx.setEnabled(True)
            self.gammaZSpinBx.setValue(info[side + '_fix_gammaz'])

        if side+'_fix_sigz' in info:
            self.fixedSigZ.setChecked(True)
            self.sigZSpnBx.setEnabled(True)
            self.sigZSpnBx.setValue(info[side+'_fix_sigz'])

        if side+'_fix_zline_EP' in info:
            self.fixedZlineEP.setChecked(True)
            self.zlineSpnBxEP.setEnabled(True)
            self.zlineSpnBxEP.setValue(info[side + '_fix_zline_EP'])

        if side+'_fix_intz_EP' in info:
            self.fixedIntZEP.setChecked(True)
            self.intZSpnBxEP.setEnabled(True)
            self.intZSpnBxEP.setValue(info[side + '_fix_intz_EP'])

        if side+'_fix_gammaz_EP' in info:
            self.fixedGammaZEP.setChecked(True)
            self.gammaZSpnBxEP.setEnabled(True)
            self.gammaZSpinBxEP.setValue(info[side + '_fix_gammaz_EP'])

        if side+'_fix_sigz_EP' in info:
            self.fixedSigZEP.setChecked(True)
            self.sigZSpnBxEP.setEnabled(True)
            self.sigZSpnBxEP.setValue(info[side+'_fix_sigz_EP'])

        self.syncUI = False

    def fixSigmaCChecked(self):
        """
        Triggered when fix sigma C is checked
        """
        side = self.side
        parent = self.parent
        if self.fixSigmaC.isChecked():
            if side+'_sigmac' in parent.bioImg.info:
                del parent.bioImg.info[side+'_sigmac']
        else:
            if side+'_fix_sigmac' in parent.bioImg.info:
                del parent.bioImg.info[side+'_fix_sigmac']
        self.sigmaCSpinBx.setEnabled(self.fixSigmaC.isChecked())
    
    def fixSigmaSChecked(self):
        """
        Triggered when fix sigma S is checked
        """
        side = self.side
        parent = self.parent
        if self.fixSigmaS.isChecked():
            if side+'_sigmas' in parent.bioImg.info:
                del parent.bioImg.info[side+'_sigmas']
        else:
            if side+'_fix_sigmas' in parent.bioImg.info:
                del parent.bioImg.info[side+'_fix_sigmas']
        self.sigmaSSpinBx.setEnabled(self.fixSigmaS.isChecked())

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

    def fixedFittingParams(self, name, elem):
        """
        Fixed Value Changed. Remove fit_results from info dict to make it be re-calculated
        """
        # self.parent.refreshAllFittingParams()
        self.log_changes(name, elem, prefix='(fitting)')

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
        if self.parent.extraPeakChkBx.isChecked():
            self.zlineSpnBxEP.setEnabled(self.fixedZlineEP.isChecked())
            self.sigZSpnBxEP.setEnabled(self.fixedSigZEP.isChecked())
            self.intZSpnBxEP.setEnabled(self.fixedIntZEP.isChecked())
            self.gammaZSpnBxEP.setEnabled(self.fixedGammaZEP.isChecked())

    def skeletalChanged(self, name, elem):
        """
        Reset all about z line and re-process image
        """
        bioImg = self.parent.bioImg
        if bioImg is None or self.syncUI:
            return
        self.log_changes(name, elem, prefix='(skeletal)')

    def hideGamma(self, flag):
        """
        Hide gamma settings if model is not Voigt
        """
        self.gammaSpinBx.setHidden(flag)
        self.fixGamma.setHidden(flag)
        self.gammaZSpnBx.setHidden(flag)
        self.fixedGammaZ.setHidden(flag)
        self.gammaZSpnBxEP.setHidden(flag)
        self.fixedGammaZEP.setHidden(flag)

    def getFittingSettings(self, first_run=False):
        """
        Get All settings that are necessary for EquatorImage to process
        :return:
        """
        settings = {}
        side = self.side

        if first_run: # To check the boxes when no cache and first img
            self.fixSigmaS.setChecked(True)
            self.sigmaSSpinBx.setEnabled(True)
            self.fixSigmaC.setChecked(True)
            self.sigmaCSpinBx.setEnabled(True)

        # get all locked parameters
        if self.fixSigmaC.isChecked():
            settings[side+'_fix_sigmac'] = self.sigmaCSpinBx.value()
        else:
            settings[side + '_sigmac'] = self.sigmaCSpinBx.value()

        if self.fixSigmaD.isChecked():
            settings[side+'_fix_sigmad'] = self.sigmaDSpinBx.value()
        else:
            settings[side + '_sigmad'] = self.sigmaDSpinBx.value()

        if self.fixSigmaS.isChecked():
            settings[side+'_fix_sigmas'] = self.sigmaSSpinBx.value()
        else:
            settings[side + '_sigmas'] = self.sigmaSSpinBx.value()

        if not self.gammaSpinBx.isHidden():
            if self.fixGamma.isChecked():
                settings[side+'_fix_gamma'] = self.gammaSpinBx.value()
            else:
                settings[side + '_gamma'] = self.gammaSpinBx.value()

        if self.parent.skeletalChkBx.isChecked():
            if self.fixedIntZ.isChecked():
                settings[side+'_fix_intz'] = self.intZSpnBx.value()
            if self.fixedSigZ.isChecked():
                settings[side+'_fix_sigz'] = self.sigZSpnBx.value()
            if self.fixedZline.isChecked():
                settings[side+'_fix_zline'] = self.zlineSpnBx.value()
            if self.fixedGammaZ.isChecked():
                settings[side + '_fix_gammaz'] = self.gammaZSpnBx.value()
            if self.parent.extraPeakChkBx.isChecked():
                if self.fixedIntZEP.isChecked():
                    settings[side+'_fix_intz_EP'] = self.intZSpnBxEP.value()
                if self.fixedSigZEP.isChecked():
                    settings[side+'_fix_sigz_EP'] = self.sigZSpnBxEP.value()
                if self.fixedZlineEP.isChecked():
                    settings[side+'_fix_zline_EP'] = self.zlineSpnBxEP.value()
                if self.fixedGammaZEP.isChecked():
                    settings[side + '_fix_gammaz_EP'] = self.gammaZSpnBxEP.value()

        return settings

    def init_logging(self):
        """
        Initialize the logging
        """
        for objName in self.editableVars:
            self.editableVars[objName] = self.findChild(QAbstractSpinBox, objName).value()
        #print(self.side, self.editableVars)

    def write_log(self, msg):
        """
        Write the log
        """
        if hasattr(self.parent.__class__, 'write_log') and \
           callable(getattr(self.parent.__class__, 'write_log')):
            self.parent.write_log(self.side + ' ' + msg)

    def log_changes(self, name, obj, prefix=''):
        """
        Change the log file and rewrite it
        """
        newValue = obj.value()
        varName = obj.objectName()
        if self.editableVars[varName] == newValue:
            return
        self.write_log(f'{prefix}{name}Changed: {self.editableVars[varName]} -> {newValue}')
        self.editableVars[varName] = newValue
