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
        
        # Backup original fit_results for Cancel operation
        self.original_fit_result = dict(
            self.projProc.info['fit_results'][self.box_name]
        )
        
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
        self.equalVarianceChkBx = QCheckBox("Equal Variance (Common Sigma)")
        # Note: Initial state will be set in populateParameters() based on fit results
        self.equalVarianceChkBx.setToolTip(
            "When checked: All peaks share one common sigma (GMM mode)\n"
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
        is_gmm = 'common_sigma' in fit_result
        # Block signals temporarily to avoid triggering stateChanged before table is populated
        self.equalVarianceChkBx.blockSignals(True)
        self.equalVarianceChkBx.setChecked(is_gmm)
        self.equalVarianceChkBx.blockSignals(False)
        
        # Extract all parameters
        params_to_show = {}
        
        # Common sigma (key GMM parameter)
        if 'common_sigma' in fit_result:
            params_to_show['common_sigma'] = {
                'val': fit_result['common_sigma'],
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
            valueSpin.valueChanged.connect(self.onParameterChanged)
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
            
            if param_name.startswith('sigma') and param_name != 'common_sigma':
                # Individual sigma parameters
                fixedItem = self.paramTable.item(row, 0)
                valueSpin = self.paramTable.cellWidget(row, 2)
                minSpin = self.paramTable.cellWidget(row, 3)
                maxSpin = self.paramTable.cellWidget(row, 4)
                
                if is_equal_variance:
                    # GMM mode: sigma fixed and bound to common sigma
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
            
            elif param_name == 'common_sigma':
                # common_sigma only visible in GMM mode
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
    
    def onParameterChanged(self):
        """
        Real-time parameter update: when user changes value in table,
        update info['fit_results'] and redraw immediately
        """
        # Get current parameters from table
        edited_params = self.getParametersFromTable()
        
        # Update info['fit_results'] directly (unified data layer)
        for param_name, pinfo in edited_params.items():
            self.projProc.info['fit_results'][self.box_name][param_name] = pinfo['val']
        
        self.parent_tab.need_update = True
        # Redraw with updated parameters (uses info['fit_results'])
        self.parent_tab.updateUI()
        
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
                    sigma_info = f"Common Sigma: {result.get('common_sigma', 0):.4f}"
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
        Refit using updated parameters by calling ProjectionProcessor.fitModel()
        This ensures consistency with initial fitting logic including hull constraints
        """
        # Check Equal Variance checkbox state
        use_equal_variance = self.equalVarianceChkBx.isChecked()
        
        print(f"\n=== DEBUG refitGMM (Unified Logic) ===")
        print(f"Box: {self.box_name}")
        print(f"use_equal_variance: {use_equal_variance}")
        print(f"Number of parameters: {len(paramInfo)}")
        
        # Update gmm_mode flag BEFORE calling fitModel
        if 'gmm_mode' not in self.projProc.info:
            self.projProc.info['gmm_mode'] = {}
        self.projProc.info['gmm_mode'][self.box_name] = use_equal_variance
        
        # Also update parent's gmm_boxes for persistence
        if hasattr(self.parent_tab.parent, 'gmm_boxes'):
            self.parent_tab.parent.gmm_boxes[self.box_name] = use_equal_variance
        
        # Extract peak positions from paramInfo (p_0, p_1, p_2, ...)
        # These are relative positions (distance from centerX)
        peak_positions = []
        i = 0
        while f'p_{i}' in paramInfo:
            peak_positions.append(paramInfo[f'p_{i}']['val'])
            i += 1
        
        if not peak_positions:
            print("ERROR: No peak positions found in paramInfo!")
            return None
        
        print(f"Extracted {len(peak_positions)} peak positions: {peak_positions[:5]}...")
        
        # Update moved_peaks with new positions
        self.projProc.info['moved_peaks'][self.box_name] = peak_positions
        
        # Update fixed_sigma if any sigma is marked as fixed in non-GMM mode
        if not use_equal_variance:
            self.projProc.fixed_sigma = {}
            for param_name, pinfo in paramInfo.items():
                if param_name.startswith('sigma') and param_name != 'common_sigma':
                    if pinfo['fixed']:
                        sigma_idx = int(param_name.replace('sigma', ''))
                        self.projProc.fixed_sigma[sigma_idx] = pinfo['val']
        else:
            self.projProc.fixed_sigma = {}
        
        # Ensure hists2 has data for this box (fitModel requires it)
        if 'hists2' not in self.projProc.info:
            self.projProc.info['hists2'] = {}
        if self.box_name not in self.projProc.info['hists2']:
            # Restore from subtracted_hists if available
            if 'subtracted_hists' in self.projProc.info and self.box_name in self.projProc.info['subtracted_hists']:
                self.projProc.info['hists2'][self.box_name] = self.projProc.info['subtracted_hists'][self.box_name]
            else:
                print(f"ERROR: No histogram data found for box {self.box_name}!")
                return None
        
        # Clear cached fit results to force re-fitting
        if self.box_name in self.projProc.info.get('fit_results', {}):
            del self.projProc.info['fit_results'][self.box_name]
        
        # Call the unified fitModel method
        # Note: fitModel processes all boxes, but will only refit boxes that don't have fit_results
        try:
            self.projProc.fitModel()
            
            # Retrieve the fit result
            if self.box_name in self.projProc.info.get('fit_results', {}):
                result_dict = self.projProc.info['fit_results'][self.box_name]
                
                # Debug: print sigma values after fit
                print(f"Fit complete. Error: {result_dict.get('error', 0):.6f}")
                if use_equal_variance:
                    print(f"  common_sigma: {result_dict.get('common_sigma', 'N/A')}")
                else:
                    sigma_values = [result_dict.get(f'sigma{i}', None) for i in range(20) if f'sigma{i}' in result_dict]
                    print(f"  Individual sigmas: {sigma_values[:5]}...")
                
                print(f"=== Refit completed using unified logic, gmm_mode = {use_equal_variance} ===\n")
                
                return result_dict
            else:
                print("ERROR: fitModel did not produce results for this box!")
                return None
                
        except Exception as e:
            print(f"ERROR in fitModel: {e}")
            import traceback
            traceback.print_exc()
            return None
    
    def onApply(self):
        """
        Apply changes and close - info is already updated by onParameterChanged
        """
        # Update gmm_mode flag to persist the equal variance setting
        use_equal_variance = self.equalVarianceChkBx.isChecked()
        if 'gmm_mode' not in self.projProc.info:
            self.projProc.info['gmm_mode'] = {}
        self.projProc.info['gmm_mode'][self.box_name] = use_equal_variance
        
        # Also update parent's gmm_boxes for persistence
        if hasattr(self.parent_tab.parent, 'gmm_boxes'):
            self.parent_tab.parent.gmm_boxes[self.box_name] = use_equal_variance
        
        # Final refresh to ensure UI is in sync
        self.parent_tab.updateUI()
        
        self.accept()
    
    def reject(self):
        """
        Cancel: restore original fit_results and close
        """
        # Restore backup to undo all changes
        self.projProc.info['fit_results'][self.box_name] = self.original_fit_result
        
        # Redraw to show original values
        self.parent_tab.updateUI()
        self.parent_tab.graphCanvas1.draw()
        self.parent_tab.graphCanvas2.draw()
        
        super().reject()
