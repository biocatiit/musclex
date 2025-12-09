"""
GMM Parameter Editor Dialog - Simple parameter editor for GMM fitting
"""

from .pyqt_utils import *
import numpy as np
from sklearn.metrics import r2_score
from lmfit import Model, Parameters


class GMMParameterEditorDialog(QDialog):
    """
    GMM parameter editor dialog - edit and refit parameters
    """
    def __init__(self, parent_tab, box_name):
        super().__init__(parent_tab)
        self.parent_tab = parent_tab
        self.box_name = box_name
        self.projProc = parent_tab.parent.projProc
        
        self.setWindowTitle(f"GMM Parameter Editor - {box_name}")
        self.resize(600, 500)  # Simpler, smaller window
        
        # Make dialog independent of parent window movement
        self.setWindowFlags(Qt.Window | Qt.WindowCloseButtonHint)
        
        self.initUI()
        self.populateParameters()
    
    def initUI(self):
        """
        Initialize UI - simple table layout
        """
        mainLayout = QVBoxLayout(self)
        
        # Title
        titleLabel = QLabel(f"<h2>Parameters for {self.box_name}</h2>")
        mainLayout.addWidget(titleLabel)
        
        # Parameter table (same as Equator)
        self.paramTable = QTableWidget()
        self.paramTable.setColumnCount(5)
        self.paramTable.setHorizontalHeaderLabels(
            ["Fixed", "Parameter", "Value", "Min", "Max"]
        )
        self.paramTable.horizontalHeader().setStretchLastSection(True)
        self.paramTable.setColumnWidth(0, 50)
        self.paramTable.setColumnWidth(1, 150)
        self.paramTable.setColumnWidth(2, 100)
        self.paramTable.setColumnWidth(3, 100)
        self.paramTable.setColumnWidth(4, 100)
        
        self.paramTable.itemClicked.connect(self.onFixedToggled)
        
        mainLayout.addWidget(self.paramTable)
        
        # Buttons
        buttonLayout = QHBoxLayout()
        
        self.refitBtn = QPushButton("Re-fit")
        self.refitBtn.clicked.connect(self.onRefit)
        
        self.applyBtn = QPushButton("Apply & Close")
        self.applyBtn.clicked.connect(self.onApply)
        
        self.cancelBtn = QPushButton("Cancel")
        self.cancelBtn.clicked.connect(self.reject)
        
        buttonLayout.addWidget(self.refitBtn)
        buttonLayout.addWidget(self.applyBtn)
        buttonLayout.addWidget(self.cancelBtn)
        
        mainLayout.addLayout(buttonLayout)
    
    def populateParameters(self):
        """
        Populate parameter table from fit results
        """
        fit_result = self.projProc.info['fit_results'][self.box_name]
        
        # Extract all parameters
        params_to_show = {}
        
        # Shared sigma (key GMM parameter)
        if 'shared_sigma' in fit_result:
            params_to_show['shared_sigma'] = {
                'val': fit_result['shared_sigma'],
                'min': 1.0,
                'max': 100.0,
                'fixed': False
            }
        
        # Parameters for each peak
        i = 0
        while f'p_{i}' in fit_result:
            # Position
            params_to_show[f'p_{i}'] = {
                'val': fit_result[f'p_{i}'],
                'min': fit_result[f'p_{i}'] - 20,
                'max': fit_result[f'p_{i}'] + 20,
                'fixed': False
            }
            
            # Amplitude
            params_to_show[f'amplitude{i}'] = {
                'val': fit_result[f'amplitude{i}'],
                'min': 0,
                'max': fit_result[f'amplitude{i}'] * 5,
                'fixed': False
            }
            
            # Sigma (display only, not editable because shared)
            if f'sigma{i}' in fit_result:
                params_to_show[f'sigma{i}'] = {
                    'val': fit_result[f'sigma{i}'],
                    'min': 1.0,
                    'max': 100.0,
                    'fixed': True  # Fixed because depends on shared_sigma
                }
            
            i += 1
        
        # Fill table
        self.paramTable.setRowCount(len(params_to_show))
        
        row = 0
        for param_name in sorted(params_to_show.keys()):
            pdict = params_to_show[param_name]
            
            # Fixed checkbox
            chkItem = QTableWidgetItem()
            chkItem.setFlags(Qt.ItemIsUserCheckable | Qt.ItemIsEnabled)
            chkItem.setCheckState(Qt.Checked if pdict['fixed'] else Qt.Unchecked)
            # If individual sigma, disable checkbox
            if param_name.startswith('sigma') and param_name != 'shared_sigma':
                chkItem.setFlags(Qt.ItemIsEnabled)
                chkItem.setCheckState(Qt.Checked)
            self.paramTable.setItem(row, 0, chkItem)
            
            # Parameter name
            self.paramTable.setItem(row, 1, QTableWidgetItem(param_name))
            
            # Value spinbox
            valueSpin = QDoubleSpinBox()
            valueSpin.setDecimals(6)
            valueSpin.setRange(-1e10, 1e10)
            valueSpin.setValue(pdict['val'])
            # Individual sigma not editable
            if param_name.startswith('sigma') and param_name != 'shared_sigma':
                valueSpin.setEnabled(False)
            self.paramTable.setCellWidget(row, 2, valueSpin)
            
            # Min spinbox
            minSpin = QDoubleSpinBox()
            minSpin.setDecimals(6)
            minSpin.setRange(-1e10, 1e10)
            minSpin.setValue(pdict['min'])
            minSpin.setEnabled(not pdict['fixed'])
            self.paramTable.setCellWidget(row, 3, minSpin)
            
            # Max spinbox
            maxSpin = QDoubleSpinBox()
            maxSpin.setDecimals(6)
            maxSpin.setRange(-1e10, 1e10)
            maxSpin.setValue(pdict['max'])
            maxSpin.setEnabled(not pdict['fixed'])
            self.paramTable.setCellWidget(row, 4, maxSpin)
            
            row += 1
    
    def onFixedToggled(self, item):
        """
        Toggle Fixed checkbox - enable/disable Min/Max
        """
        if item.column() == 0:
            row = item.row()
            minSpin = self.paramTable.cellWidget(row, 3)
            maxSpin = self.paramTable.cellWidget(row, 4)
            
            if item.checkState() == Qt.Checked:
                minSpin.setEnabled(False)
                maxSpin.setEnabled(False)
            else:
                minSpin.setEnabled(True)
                maxSpin.setEnabled(True)
    
    def getParametersFromTable(self):
        """
        Extract parameters from table
        """
        params_info = {}
        
        for row in range(self.paramTable.rowCount()):
            param_name = self.paramTable.item(row, 1).text()
            fixed_item = self.paramTable.item(row, 0)
            
            params_info[param_name] = {
                'fixed': fixed_item.checkState() == Qt.Checked if fixed_item else True,
                'val': self.paramTable.cellWidget(row, 2).value(),
                'min': self.paramTable.cellWidget(row, 3).value(),
                'max': self.paramTable.cellWidget(row, 4).value()
            }
        
        return params_info
    
    def onRefit(self):
        """
        Refit using current parameters and refresh parent BoxTab
        """
        QApplication.setOverrideCursor(Qt.WaitCursor)
        
        try:
            # Get parameters from table
            params_info = self.getParametersFromTable()
            
            # Call refit
            result = self.refitGMM(params_info)
            
            if result:
                # Update table with new values
                self.populateParameters()
                
                # Refresh parent BoxTab to show updated fit
                self.parent_tab.updateUI()
                
                QMessageBox.information(self, "Refit Complete", 
                                      f"New error: {result.get('error', 0):.6f}")
        except Exception as e:
            QMessageBox.critical(self, "Refit Error", str(e))
            import traceback
            traceback.print_exc()
        finally:
            QApplication.restoreOverrideCursor()
    
    def refitGMM(self, paramInfo):
        """
        Refit GMM using updated parameters
        """
        hist = np.array(self.projProc.info['subtracted_hists'][self.box_name])
        x = np.arange(0, len(hist))
        
        # Build lmfit parameters
        params = Parameters()
        int_vars = {}
        
        # centerX fixed
        box = self.projProc.info['boxes'][self.box_name]
        if self.projProc.info['types'][self.box_name] == 'h':
            centerX = self.projProc.info['centerx'] - box[0][0]
        else:
            centerX = self.projProc.info['centery'] - box[1][0]
        params.add('centerX', int(round(centerX)), vary=False)
        
        # Background parameters (from original fit_results)
        fit_result = self.projProc.info['fit_results'][self.box_name]
        for bg_param in ['bg_line', 'bg_sigma', 'bg_amplitude',
                        'center_sigma1', 'center_amplitude1',
                        'center_sigma2', 'center_amplitude2']:
            if bg_param in fit_result:
                int_vars[bg_param] = fit_result[bg_param]
        
        # GMM parameters
        # shared_sigma
        if 'shared_sigma' in paramInfo:
            pinfo = paramInfo['shared_sigma']
            if pinfo['fixed']:
                int_vars['shared_sigma'] = pinfo['val']
            else:
                params.add('shared_sigma', pinfo['val'], min=pinfo['min'], max=pinfo['max'])
        
        # Parameters for each peak
        for param_name, pinfo in paramInfo.items():
            if param_name.startswith('p_') or param_name.startswith('amplitude'):
                if pinfo['fixed']:
                    int_vars[param_name] = pinfo['val']
                else:
                    params.add(param_name, pinfo['val'], min=pinfo['min'], max=pinfo['max'])
            elif param_name.startswith('sigma') and param_name != 'shared_sigma':
                # Individual sigma bound with expression
                if 'shared_sigma' in params:
                    params.add(param_name, expr='shared_sigma')
                elif 'shared_sigma' in int_vars:
                    int_vars[param_name] = int_vars['shared_sigma']
        
        # Fit
        from ..modules.ProjectionProcessor import layerlineModel
        int_vars['x'] = x
        model = Model(layerlineModel, nan_policy='propagate', independent_vars=int_vars.keys())
        result = model.fit(hist, params=params, **int_vars)
        
        if result:
            result_dict = result.values
            int_vars.pop('x')
            result_dict.update(int_vars)
            
            fitted_curve = layerlineModel(x, **result_dict)
            result_dict['error'] = 1.0 - r2_score(hist, fitted_curve)
            
            # Update stored results
            self.projProc.info['fit_results'][self.box_name] = result_dict
            
            return result_dict
        
        return None
    
    def onApply(self):
        """
        Apply changes and close - refresh parent BoxTab
        """
        # Refresh parent window to show any manual edits
        self.parent_tab.updateUI()
        
        self.accept()
