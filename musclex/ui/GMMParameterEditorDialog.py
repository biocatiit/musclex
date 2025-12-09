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
        
        # Equal Variance checkbox
        self.equalVarianceChkBx = QCheckBox("Equal Variance (Shared Sigma)")
        # Note: Initial state will be set in populateParameters() based on fit results
        self.equalVarianceChkBx.setToolTip(
            "When checked: All peaks share one sigma (GMM mode)\n"
            "When unchecked: Each peak has independent sigma"
        )
        self.equalVarianceChkBx.stateChanged.connect(self.onEqualVarianceChanged)
        mainLayout.addWidget(self.equalVarianceChkBx)
        
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
        
        # Detect if current fit is GMM mode and set checkbox state
        is_gmm = 'shared_sigma' in fit_result
        # Block signals temporarily to avoid triggering stateChanged before table is populated
        self.equalVarianceChkBx.blockSignals(True)
        self.equalVarianceChkBx.setChecked(is_gmm)
        self.equalVarianceChkBx.blockSignals(False)
        
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
            
            # Sigma
            if f'sigma{i}' in fit_result:
                params_to_show[f'sigma{i}'] = {
                    'val': fit_result[f'sigma{i}'],
                    'min': 1.0,
                    'max': 100.0,
                    'fixed': is_gmm  # Fixed in GMM mode, editable otherwise
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
            self.paramTable.setItem(row, 0, chkItem)
            
            # Parameter name
            self.paramTable.setItem(row, 1, QTableWidgetItem(param_name))
            
            # Value spinbox
            valueSpin = QDoubleSpinBox()
            valueSpin.setDecimals(6)
            valueSpin.setRange(-1e10, 1e10)
            valueSpin.setValue(pdict['val'])
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
        
        # Apply initial editability based on checkbox state
        self.onEqualVarianceChanged(self.equalVarianceChkBx.checkState())
    
    def onEqualVarianceChanged(self, state):
        """
        Handle Equal Variance checkbox state change
        Update sigma parameters editability
        """
        is_equal_variance = (state == Qt.Checked)
        
        # Update sigma parameters editability
        for row in range(self.paramTable.rowCount()):
            param_name = self.paramTable.item(row, 1).text()
            
            if param_name.startswith('sigma') and param_name != 'shared_sigma':
                # Individual sigma parameters
                fixedItem = self.paramTable.item(row, 0)
                valueSpin = self.paramTable.cellWidget(row, 2)
                minSpin = self.paramTable.cellWidget(row, 3)
                maxSpin = self.paramTable.cellWidget(row, 4)
                
                if is_equal_variance:
                    # GMM mode: sigma fixed and bound to shared_sigma
                    fixedItem.setFlags(Qt.ItemIsEnabled)
                    fixedItem.setCheckState(Qt.Checked)
                    valueSpin.setEnabled(False)
                    minSpin.setEnabled(False)
                    maxSpin.setEnabled(False)
                else:
                    # Non-GMM mode: sigma independent and editable
                    fixedItem.setFlags(Qt.ItemIsUserCheckable | Qt.ItemIsEnabled)
                    fixedItem.setCheckState(Qt.Unchecked)
                    valueSpin.setEnabled(True)
                    minSpin.setEnabled(True)
                    maxSpin.setEnabled(True)
            
            elif param_name == 'shared_sigma':
                # shared_sigma only visible in GMM mode
                self.paramTable.setRowHidden(row, not is_equal_variance)
    
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
                
                # Force canvas redraw
                self.parent_tab.graphCanvas1.draw()
                self.parent_tab.graphCanvas2.draw()
                
                # Refresh parent BoxTab to show updated fit
                self.parent_tab.updateUI()
                
                # Display mode in message
                mode = "GMM (Equal Variance)" if self.equalVarianceChkBx.isChecked() \
                       else "Independent Sigma"
                
                # Show sigma values for comparison
                sigma_info = ""
                if self.equalVarianceChkBx.isChecked():
                    sigma_info = f"Shared Sigma: {result.get('shared_sigma', 0):.4f}"
                else:
                    sigmas = [result.get(f'sigma{i}', None) for i in range(5) if f'sigma{i}' in result]
                    sigma_info = f"Sigmas (first 5): {[f'{s:.2f}' for s in sigmas if s]}"
                
                QMessageBox.information(self, "Refit Complete", 
                                      f"Mode: {mode}\n"
                                      f"{sigma_info}\n"
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
        
        # Check Equal Variance checkbox state
        use_equal_variance = self.equalVarianceChkBx.isChecked()
        
        print(f"\n=== DEBUG refitGMM ===")
        print(f"Box: {self.box_name}")
        print(f"use_equal_variance: {use_equal_variance}")
        print(f"Number of parameters: {len(paramInfo)}")
        
        if use_equal_variance:
            # GMM mode: shared sigma
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
                    # Individual sigma bound to shared_sigma
                    if 'shared_sigma' in params:
                        params.add(param_name, expr='shared_sigma')
                    elif 'shared_sigma' in int_vars:
                        int_vars[param_name] = int_vars['shared_sigma']
        else:
            # Non-GMM mode: independent sigmas
            for param_name, pinfo in paramInfo.items():
                if param_name == 'shared_sigma':
                    continue  # Skip shared_sigma in non-GMM mode
                
                if param_name.startswith('p_') or \
                   param_name.startswith('amplitude') or \
                   param_name.startswith('sigma'):
                    if pinfo['fixed']:
                        int_vars[param_name] = pinfo['val']
                    else:
                        params.add(param_name, pinfo['val'], min=pinfo['min'], max=pinfo['max'])
        
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
            
            # Debug: print sigma values after fit
            print(f"Fit complete. Error: {result_dict['error']:.6f}")
            if use_equal_variance:
                print(f"  shared_sigma: {result_dict.get('shared_sigma', 'N/A')}")
            sigma_values = [result_dict.get(f'sigma{i}', None) for i in range(20) if f'sigma{i}' in result_dict]
            print(f"  Individual sigmas: {sigma_values[:5]}...")  # Show first 5
            
            # Update stored results
            self.projProc.info['fit_results'][self.box_name] = result_dict
            
            # Update gmm_mode flag to match current fitting mode
            if 'gmm_mode' not in self.projProc.info:
                self.projProc.info['gmm_mode'] = {}
            self.projProc.info['gmm_mode'][self.box_name] = use_equal_variance
            
            # Also update parent's gmm_boxes for persistence
            if hasattr(self.parent_tab.parent, 'gmm_boxes'):
                self.parent_tab.parent.gmm_boxes[self.box_name] = use_equal_variance
            
            # Clear cached hists2 to force UI update
            if 'hists2' in self.projProc.info and self.box_name in self.projProc.info['hists2']:
                del self.projProc.info['hists2'][self.box_name]
            
            print(f"=== Refit completed, gmm_mode set to: {use_equal_variance} ===\n")
            
            return result_dict
        
        return None
    
    def onApply(self):
        """
        Apply changes and close - refresh parent BoxTab
        """
        # Refresh parent window to show any manual edits
        self.parent_tab.updateUI()
        
        self.accept()
