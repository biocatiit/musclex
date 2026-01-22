"""
GMM Parameter Editor Dialog - Simple parameter editor for GMM fitting
"""

from .pyqt_utils import *
import copy
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
        
        # Create working copy from original fit_results (for real-time preview editing)
        box = self.projProc.boxes[self.box_name]
        if box.fit_results is None:
            raise ValueError(f"No fit results found for box '{box_name}'. Please fit peaks first.")
        
        # Working parameters - used for preview, only committed on Refit
        self.working_params = dict(box.fit_results)
        
        # Working hull_range - also needs preview support (not part of fit_results)
        self.working_hull_range = box.hull_range if box.hull_range else None
        
        # Enable preview mode on parent tab
        self.parent_tab.preview_params = self.working_params
        self.parent_tab.preview_hull_range = self.working_hull_range
        
        self.setWindowTitle(f"GMM Parameter Editor - {box_name}")
        self.resize(600, 500)  # Simpler, smaller window
        
        # Make dialog independent of parent window movement
        self.setWindowFlags(Qt.Window | Qt.WindowCloseButtonHint | Qt.WindowStaysOnTopHint)
        
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
        
        # Controls layout (Equal Variance + Hull Range)
        controlsLayout = QVBoxLayout()
        
        # Equal Variance checkbox
        self.equalVarianceChkBx = QCheckBox("Equal Variance (Common Sigma)")
        # Note: Initial state will be set in populateParameters() based on fit results
        self.equalVarianceChkBx.setToolTip(
            "When checked: All peaks share one common sigma (GMM mode)\n"
            "When unchecked: Each peak has independent sigma"
        )
        self.equalVarianceChkBx.stateChanged.connect(self.onEqualVarianceChanged)
        controlsLayout.addWidget(self.equalVarianceChkBx)
        
        # Hull Range controls
        hullLayout = QHBoxLayout()
        hullLabel = QLabel("Hull Range (Convex Hull Constraint):")
        hullLayout.addWidget(hullLabel)
        
        hullLayout.addWidget(QLabel("Start:"))
        self.hullStartSpinBox = QDoubleSpinBox()
        self.hullStartSpinBox.setDecimals(2)
        self.hullStartSpinBox.setRange(0, 1000)
        self.hullStartSpinBox.setValue(0)
        self.hullStartSpinBox.valueChanged.connect(self.onHullRangeChanged)
        self.hullStartSpinBox.setToolTip("Start position for convex hull background subtraction")
        hullLayout.addWidget(self.hullStartSpinBox)
        
        hullLayout.addWidget(QLabel("End:"))
        self.hullEndSpinBox = QDoubleSpinBox()
        self.hullEndSpinBox.setDecimals(2)
        self.hullEndSpinBox.setRange(0, 1000)
        self.hullEndSpinBox.setValue(0)
        self.hullEndSpinBox.valueChanged.connect(self.onHullRangeChanged)
        self.hullEndSpinBox.setToolTip("End position for convex hull background subtraction")
        hullLayout.addWidget(self.hullEndSpinBox)
        
        hullLayout.addStretch()
        controlsLayout.addLayout(hullLayout)
        
        mainLayout.addLayout(controlsLayout)
        
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
        
        self.refitBtn = QPushButton("Refit && Save")
        self.refitBtn.clicked.connect(self.onRefit)
        self.refitBtn.setAutoDefault(False)
        self.refitBtn.setDefault(False)
        self.refitBtn.setToolTip("Re-fit parameters and save changes to fit_results.\nYou can continue editing after saving.")
        
        self.cancelBtn = QPushButton("Close")
        self.cancelBtn.clicked.connect(self.reject)
        self.cancelBtn.setAutoDefault(False)
        self.cancelBtn.setDefault(False)
        self.cancelBtn.setToolTip("Close dialog and discard any unsaved edits.\n(Already saved changes from 'Refit & Save' will be kept)")
        
        buttonLayout.addWidget(self.refitBtn)
        buttonLayout.addWidget(self.cancelBtn)
        
        mainLayout.addLayout(buttonLayout)
    
    def populateParameters(self):
        """
        Populate parameter table from working parameters
        """
        # Use working_params for display (preview mode)
        fit_result = self.working_params
        
        # Use explicit flag to determine GMM mode (fallback to checking common_sigma)
        is_gmm = fit_result.get('use_common_sigma', 'common_sigma' in fit_result)
        
        # Block signals temporarily to avoid triggering stateChanged before table is populated
        self.equalVarianceChkBx.blockSignals(True)
        self.equalVarianceChkBx.setChecked(is_gmm)
        self.equalVarianceChkBx.blockSignals(False)
        
        # Extract all parameters
        params_to_show = {}
        
        # Per-parameter bounds (scheme B): persisted in box.param_bounds
        if self.box_name in self.projProc.boxes:
            box_bounds = self.projProc.boxes[self.box_name].param_bounds
        else:
            box_bounds = {}
        if not isinstance(box_bounds, dict):
            box_bounds = {}
        
        # Peak tolerance (fallback for initial bounds if none are stored)
        if self.box_name in self.projProc.boxes:
            peak_tol = self.projProc.boxes[self.box_name].peak_tolerance
        else:
            peak_tol = 2.0
        
        # Populate hull range spinboxes (not in table - these are preprocessing constraints)
        if self.working_hull_range and isinstance(self.working_hull_range, (tuple, list)) and len(self.working_hull_range) >= 2:
            self.hullStartSpinBox.blockSignals(True)
            self.hullEndSpinBox.blockSignals(True)
            self.hullStartSpinBox.setValue(float(self.working_hull_range[0]))
            self.hullEndSpinBox.setValue(float(self.working_hull_range[1]))
            self.hullStartSpinBox.blockSignals(False)
            self.hullEndSpinBox.blockSignals(False)
            # Enable hull range controls
            self.hullStartSpinBox.setEnabled(True)
            self.hullEndSpinBox.setEnabled(True)
        else:
            # Disable if no hull range
            self.hullStartSpinBox.setEnabled(False)
            self.hullEndSpinBox.setEnabled(False)
        
        # Common sigma - ALWAYS show it (will be enabled/disabled based on mode)
        cs_bounds = box_bounds.get('common_sigma', {})
        if not isinstance(cs_bounds, dict):
            cs_bounds = {}
        params_to_show['common_sigma'] = {
            'val': fit_result.get('common_sigma', 10.0),
            # Prefer persisted bounds; fallback to legacy UI range
            'min': cs_bounds.get('min', 1.0),
            'max': cs_bounds.get('max', 100.0),
            'fixed': fit_result.get('common_sigma_fixed', False),  # Read fixed state
            'enabled': is_gmm  # Only enabled in GMM mode
        }
        
        # Parameters for each peak
        i = 0
        while f'p_{i}' in fit_result:
            # Position
            p_name = f'p_{i}'
            stored_p_bounds = box_bounds.get(p_name, {})
            if not isinstance(stored_p_bounds, dict):
                stored_p_bounds = {}
            p_min = stored_p_bounds.get('min', fit_result[p_name] - peak_tol)
            p_max = stored_p_bounds.get('max', fit_result[p_name] + peak_tol)

            params_to_show[f'p_{i}'] = {
                'val': fit_result[f'p_{i}'],
                'min': p_min,
                'max': p_max,
                'fixed': fit_result.get(f'p_{i}_fixed', False),  # Read fixed state
                'enabled': True
            }
            
            # Amplitude
            a_name = f'amplitude{i}'
            a_bounds = box_bounds.get(a_name, {})
            if not isinstance(a_bounds, dict):
                a_bounds = {}
            params_to_show[f'amplitude{i}'] = {
                'val': fit_result[f'amplitude{i}'],
                # Prefer persisted bounds; fallback to legacy UI heuristic
                'min': a_bounds.get('min', 0),
                'max': a_bounds.get('max', fit_result[f'amplitude{i}'] * 5),
                'fixed': fit_result.get(f'amplitude{i}_fixed', False),  # Read fixed state
                'enabled': True
            }
            
            # Sigma - ALWAYS show it (will be enabled/disabled based on mode)
            s_name = f'sigma{i}'
            s_bounds = box_bounds.get(s_name, {})
            if not isinstance(s_bounds, dict):
                s_bounds = {}
            params_to_show[f'sigma{i}'] = {
                'val': fit_result.get(s_name, fit_result.get('common_sigma', 10.0)),
                # Prefer persisted bounds; fallback to legacy UI range
                'min': s_bounds.get('min', 1.0),
                'max': s_bounds.get('max', 100.0),
                'fixed': fit_result.get(f'sigma{i}_fixed', False),  # Read fixed state
                'enabled': not is_gmm  # Only enabled in non-GMM mode
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
            valueSpin.setEnabled(pdict['enabled'])
            
            # Connect signal for real-time updates
            if param_name == 'common_sigma':
                valueSpin.valueChanged.connect(self.onCommonSigmaChanged)
            else:
                valueSpin.valueChanged.connect(self.onParameterChanged)
            
            self.paramTable.setCellWidget(row, 2, valueSpin)
            
            # Min spinbox
            minSpin = QDoubleSpinBox()
            minSpin.setDecimals(6)
            minSpin.setRange(-1e10, 1e10)
            minSpin.setValue(pdict['min'])
            minSpin.setEnabled(pdict['enabled'] and not pdict['fixed'])
            self.paramTable.setCellWidget(row, 3, minSpin)
            
            # Max spinbox
            maxSpin = QDoubleSpinBox()
            maxSpin.setDecimals(6)
            maxSpin.setRange(-1e10, 1e10)
            maxSpin.setValue(pdict['max'])
            maxSpin.setEnabled(pdict['enabled'] and not pdict['fixed'])
            self.paramTable.setCellWidget(row, 4, maxSpin)
            
            row += 1
        
        # Don't call onEqualVarianceChanged here, state is already set correctly
    
    def onEqualVarianceChanged(self, state):
        """
        Handle Equal Variance checkbox state change
        - GMM mode (checked): common_sigma enabled, individual sigmas disabled and sync to common_sigma
        - Non-GMM mode (unchecked): common_sigma disabled, individual sigmas enabled
        """
        # Use isChecked() instead of comparing state, more reliable
        is_gmm = self.equalVarianceChkBx.isChecked()
        
        # Get common_sigma value for syncing
        common_sigma_value = None
        for row in range(self.paramTable.rowCount()):
            if self.paramTable.item(row, 1).text() == 'common_sigma':
                common_sigma_value = self.paramTable.cellWidget(row, 2).value()
                break
        
        # Update all parameters
        for row in range(self.paramTable.rowCount()):
            param_name = self.paramTable.item(row, 1).text()
            valueSpin = self.paramTable.cellWidget(row, 2)
            minSpin = self.paramTable.cellWidget(row, 3)
            maxSpin = self.paramTable.cellWidget(row, 4)
            fixedItem = self.paramTable.item(row, 0)
            
            if param_name == 'common_sigma':
                # Common sigma: enabled in GMM mode, disabled otherwise
                valueSpin.setEnabled(is_gmm)
                minSpin.setEnabled(is_gmm)
                maxSpin.setEnabled(is_gmm)
                
            elif param_name.startswith('sigma') and param_name != 'common_sigma':
                # Individual sigmas
                if is_gmm:
                    # GMM mode: disable and sync to common_sigma
                    valueSpin.setEnabled(False)
                    minSpin.setEnabled(False)
                    maxSpin.setEnabled(False)
                    if common_sigma_value is not None:
                        valueSpin.blockSignals(True)
                        valueSpin.setValue(common_sigma_value)
                        valueSpin.blockSignals(False)
                else:
                    # Non-GMM mode: enable for independent editing
                    valueSpin.setEnabled(True)
                    minSpin.setEnabled(True)
                    maxSpin.setEnabled(True)
        
        # Update use_common_sigma flag in working_params (preview mode)
        self.working_params['use_common_sigma'] = is_gmm
        
        # Update parameter values and redraw
        self.onParameterChanged()
    
    def onHullRangeChanged(self):
        """
        Handle hull range spinbox changes - update working_hull_range and preview
        """
        hull_start = self.hullStartSpinBox.value()
        hull_end = self.hullEndSpinBox.value()
        
        self.working_hull_range = (hull_start, hull_end)
        self.parent_tab.preview_hull_range = self.working_hull_range
        
        # Trigger redraw
        self.parent_tab.need_update = True
        self.parent_tab.updateUI()
    
    def _get_peak_tolerance(self) -> float:
        """
        Get current peak tolerance (used for p_i bounds reset).
        Prefer the live value from the parent tab if available.
        """
        try:
            if hasattr(self.parent_tab, 'peakToleranceSpinBox'):
                return float(self.parent_tab.peakToleranceSpinBox.value())
        except Exception:
            pass
        if self.box_name in self.projProc.boxes:
            return float(self.projProc.boxes[self.box_name].peak_tolerance)
        return 2.0
    
    def _get_sigma_tolerance(self) -> float:
        """
        Get current sigma tolerance (used for sigma/common_sigma bounds reset).
        Prefer the live value from the parent tab if available.
        """
        try:
            if hasattr(self.parent_tab, 'sigmaToleranceSpinBox'):
                return float(self.parent_tab.sigmaToleranceSpinBox.value())
        except Exception:
            pass
        if self.box_name in self.projProc.boxes:
            return float(self.projProc.boxes[self.box_name].sigma_tolerance)
        return 5.0
    
    def _find_row_for_value_widget(self, widget):
        """
        Find row index in paramTable whose Value cell widget equals the given widget.
        Returns row index or None.
        """
        for row in range(self.paramTable.rowCount()):
            if self.paramTable.cellWidget(row, 2) is widget:
                return row
        return None
    
    def _reset_bounds_for_row_by_tolerance(self, row: int, param_name: str, value: float):
        """
        Reset Min/Max in a given row based on current tolerance:
        - p_i: value ± peak_tolerance
        - sigma{i}, common_sigma: value ± sigma_tolerance (clamped at 0 for min)
        """
        if row is None or row < 0 or row >= self.paramTable.rowCount():
            return
        minSpin = self.paramTable.cellWidget(row, 3)
        maxSpin = self.paramTable.cellWidget(row, 4)
        if minSpin is None or maxSpin is None:
            return
        
        if param_name.startswith('p_'):
            tol = self._get_peak_tolerance()
            bmin = float(value) - tol
            bmax = float(value) + tol
        elif param_name == 'common_sigma' or (param_name.startswith('sigma') and param_name != 'common_sigma'):
            tol = self._get_sigma_tolerance()
            bmin = max(0.0, float(value) - tol)
            bmax = float(value) + tol
        else:
            return
        
        if bmin > bmax:
            bmin, bmax = bmax, bmin
        if bmin == bmax:
            bmin = max(0.0, bmin - 0.5)
            bmax = bmax + 0.5
        
        # Update UI without triggering extra cascades
        try:
            minSpin.blockSignals(True)
            maxSpin.blockSignals(True)
            minSpin.setValue(bmin)
            maxSpin.setValue(bmax)
        finally:
            minSpin.blockSignals(False)
            maxSpin.blockSignals(False)
    
    def updatePeakBounds(self, peak_indices):
        """
        Update Min/Max bounds for specific peaks after their positions have changed.
        This is called during drag operations to keep bounds synchronized with peak positions.
        
        :param peak_indices: List of peak indices (e.g., [0, 1, 2] for p_0, p_1, p_2)
        """
        for row in range(self.paramTable.rowCount()):
            param_name_item = self.paramTable.item(row, 1)
            if param_name_item is None:
                continue
            param_name = param_name_item.text()
            
            # Check if this is one of the affected peaks
            if param_name.startswith('p_'):
                try:
                    peak_idx = int(param_name.split('_')[1])
                    if peak_idx in peak_indices:
                        # Get current value from working_params (already updated by drag)
                        current_value = self.working_params.get(param_name)
                        if current_value is not None:
                            # Update value display in table
                            valueSpin = self.paramTable.cellWidget(row, 2)
                            if valueSpin is not None:
                                valueSpin.blockSignals(True)
                                valueSpin.setValue(current_value)
                                valueSpin.blockSignals(False)
                            # Reset bounds based on new value
                            self._reset_bounds_for_row_by_tolerance(row, param_name, current_value)
                except (ValueError, IndexError):
                    continue
    
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
        update working_params for preview (does NOT modify fit_results)
        """
        # If a value spinner changed for p_i / sigma_i, auto-reset that row's bounds to value ± tolerance
        sender = self.sender()
        if isinstance(sender, QDoubleSpinBox):
            row = self._find_row_for_value_widget(sender)
            if row is not None:
                try:
                    param_name = self.paramTable.item(row, 1).text()
                    if param_name.startswith('p_') or param_name.startswith('sigma'):
                        self._reset_bounds_for_row_by_tolerance(row, param_name, sender.value())
                except Exception:
                    pass
        
        # Get current parameters from table (model parameters only)
        edited_params = self.getParametersFromTable()
        
        # Update working_params (preview mode - not committed yet)
        for param_name, pinfo in edited_params.items():
            self.working_params[param_name] = pinfo['val']
        
        # parent_tab.preview_params points to working_params, already in sync
        # Trigger redraw (updateUI will use preview_params via _getRenderParams())
        self.parent_tab.need_update = True
        self.parent_tab.updateUI()
    
    def _commit_bounds_from_table(self, params_info):
        """
        Commit Min/Max bounds from table to ProcessingBox.param_bounds.
        This is used right before refit so fitModel uses the user's edited bounds.
        """
        box = self.projProc.boxes[self.box_name]
        
        for param_name, pinfo in params_info.items():
            # Only commit if min/max exist in the table structure
            box.param_bounds[param_name] = {
                'min': float(pinfo.get('min', 0.0)),
                'max': float(pinfo.get('max', 0.0)),
            }
    
    def onCommonSigmaChanged(self, value):
        """
        When common_sigma changes in GMM mode, sync all individual sigmas
        (Updates working_params only for preview)
        """
        # Auto-reset common_sigma bounds to value ± sigma tolerance
        try:
            row = self._find_row_for_value_widget(self.sender())
            if row is not None:
                self._reset_bounds_for_row_by_tolerance(row, 'common_sigma', value)
        except Exception:
            pass
        
        # Update working_params (preview mode)
        self.working_params['common_sigma'] = value
        
        if self.equalVarianceChkBx.isChecked():
            # GMM mode: sync all individual sigmas to common_sigma
            for row in range(self.paramTable.rowCount()):
                param_name = self.paramTable.item(row, 1).text()
                if param_name.startswith('sigma') and param_name != 'common_sigma':
                    valueSpin = self.paramTable.cellWidget(row, 2)
                    valueSpin.blockSignals(True)  # Avoid triggering cascade
                    valueSpin.setValue(value)
                    valueSpin.blockSignals(False)
                    # Keep sigma bounds visually in sync as well
                    self._reset_bounds_for_row_by_tolerance(row, param_name, value)
                    # Update in working_params
                    self.working_params[param_name] = value
        
        # Trigger redraw (uses preview_params)
        self.parent_tab.need_update = True
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
            
            # Commit bounds BEFORE refit so ProjectionProcessor.fitModel uses them
            self._commit_bounds_from_table(params_info)
            
            # Call refit
            result = self.refitGMM(params_info)
            
            if result:
                # Refit successful - sync working_params to new fit_results
                self.working_params = dict(result)
                self.parent_tab.preview_params = self.working_params
                
                # Also sync working_hull_range if it was updated
                box = self.projProc.boxes[self.box_name]
                if box.hull_range:
                    self.working_hull_range = box.hull_range
                    self.parent_tab.preview_hull_range = self.working_hull_range
                    
                    # Update snapshot so Close won't revert Refitted changes
                    import copy
                    if hasattr(self.parent_tab, 'snapshot_hull_range'):
                        self.parent_tab.snapshot_hull_range = copy.deepcopy(box.hull_range)
                    
                    # Update zoom to match new hull_range
                    if self.parent_tab.zoom1 is not None:
                        self.parent_tab.autoZoomToHullRange()
                
                # Also update peaks snapshot
                if box.peaks:
                    import copy
                    if hasattr(self.parent_tab, 'snapshot_peaks'):
                        self.parent_tab.snapshot_peaks = copy.deepcopy(box.peaks)
                
                # Update table with new values
                self.populateParameters()
                
                # Set need_update flag to ensure UI refresh
                self.parent_tab.need_update = True
                
                # Refresh parent BoxTab to show updated fit
                self.parent_tab.updateUI()
                
                # Force canvas redraw after updateUI
                self.parent_tab.graphCanvas1.draw()
                self.parent_tab.graphCanvas2.draw()
                
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
                QApplication.restoreOverrideCursor()
                QMessageBox.information(self, "Refit & Save Complete", 
                                      f"✓ Changes saved to fit_results!\n\n"
                                      f"Mode: {mode}\n"
                                      f"{sigma_info}\n"
                                      f"Fit Error: {result.get('error', 0):.6f}\n\n"
                                      f"You can:\n"
                                      f"• Continue editing and save again\n"
                                      f"• Click 'Close' (unsaved edits will be discarded)")
        except Exception as e:
            QApplication.restoreOverrideCursor()
            QMessageBox.critical(self, "Refit Error", str(e))
            import traceback
            traceback.print_exc()
    
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
        
        # Get ProcessingBox for direct updates
        box = self.projProc.boxes[self.box_name]
        
        # Save old result for comparison
        old_result = None
        if box.fit_results:
            old_result = dict(box.fit_results)
            print(f"\n--- OLD VALUES (before refit) ---")
            print(f"  common_sigma: {old_result.get('common_sigma', 'N/A')}")
            print(f"  error: {old_result.get('error', 'N/A')}")
            if use_equal_variance:
                print(f"  (GMM mode - all sigmas share common_sigma)")
            else:
                sigmas = [old_result.get(f'sigma{i}', None) for i in range(5) if f'sigma{i}' in old_result]
                print(f"  individual sigmas (first 5): {sigmas}")
        
        # Update use_common_sigma flag BEFORE calling fitModel
        box.use_common_sigma = use_equal_variance
        
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
        
        # Get hull_range from spinboxes (not from paramInfo - it's a preprocessing constraint)
        hull_range_changed = False
        if self.hullStartSpinBox.isEnabled():
            hull_start = self.hullStartSpinBox.value()
            hull_end = self.hullEndSpinBox.value()
            new_hull_range = (hull_start, hull_end)
            
            # Check if hull_range actually changed
            old_hull_range = box.hull_range
            if old_hull_range != new_hull_range:
                hull_range_changed = True
                box.hull_range = new_hull_range
                print(f"Updated hull_range: {old_hull_range} -> {new_hull_range}")
        
        # Update both peaks and moved_peaks with new positions (do this BEFORE clearing cache)
        # peaks is used by applyConvexhull, moved_peaks is used by fitModel
        box.peaks = peak_positions
        box.moved_peaks = peak_positions
        
        # Update fixed parameters for this box (per-box fixed storage)
        if not isinstance(self.projProc.fixed_sigma, dict):
            self.projProc.fixed_sigma = {}
        if not isinstance(self.projProc.fixed_center, dict):
            self.projProc.fixed_center = {}
        if not isinstance(self.projProc.fixed_amplitude, dict):
            self.projProc.fixed_amplitude = {}
        if not isinstance(self.projProc.fixed_common_sigma, dict):
            self.projProc.fixed_common_sigma = {}
        
        # Clear only this box's fixed state to avoid affecting other boxes
        self.projProc.fixed_sigma[self.box_name] = {}
        self.projProc.fixed_center[self.box_name] = {}
        self.projProc.fixed_amplitude[self.box_name] = {}
        if self.box_name in self.projProc.fixed_common_sigma:
            del self.projProc.fixed_common_sigma[self.box_name]
        
        for param_name, pinfo in paramInfo.items():
            if not pinfo.get('fixed', False):
                continue
            
            # Fixed common_sigma (GMM mode)
            if param_name == 'common_sigma':
                self.projProc.fixed_common_sigma[self.box_name] = pinfo['val']
            
            # Fixed sigma (independent mode)
            elif param_name.startswith('sigma') and param_name != 'common_sigma':
                sigma_idx = int(param_name.replace('sigma', ''))
                self.projProc.fixed_sigma[self.box_name][sigma_idx] = pinfo['val']
            
            # Fixed position (p_0, p_1, ...)
            elif param_name.startswith('p_'):
                pos_idx = int(param_name.replace('p_', ''))
                self.projProc.fixed_center[self.box_name][pos_idx] = pinfo['val']
            
            # Fixed amplitude
            elif param_name.startswith('amplitude'):
                amp_idx = int(param_name.replace('amplitude', ''))
                self.projProc.fixed_amplitude[self.box_name][amp_idx] = pinfo['val']
        
        # Clear cached results to force re-fitting and recalculation
        # Note: We delete fit_results and subtracted_hists to trigger recalculation
        # hists2 is kept UNLESS hull_range changed (which requires recalculating background)
        # baselines, centroids, etc. will be automatically deleted by getPeakInfos() 
        # through its internal removeInfo() calls
        
        box.fit_results = None
        box.subtracted_hist = None
        
        # If hull_range changed, clear hists2 and related data to force recalculation
        if hull_range_changed:
            box.hist2 = None
            box.baselines = None
            box.centroids = None
            box.widths = None
            box.areas = None
            print(f"Cleared hists2 and dependent data for {self.box_name} due to hull_range change")
        
        # DO NOT delete moved_peaks here - we just set it above and fitModel needs it!
        
        # Debug: Check what data exists before calling fitModel
        print(f"\n--- Data state BEFORE fitModel ---")
        print(f"  peaks: {box.peaks}")
        print(f"  moved_peaks: {box.moved_peaks}")
        print(f"  hist exists: {box.hist is not None}")
        print(f"  hist2 exists: {box.hist2 is not None}")
        print(f"  fit_results exists: {box.fit_results is not None}")
        print(f"  bgsub: {box.bgsub}")
        
        # Call the complete processing pipeline (same flow as process())
        # Note: These methods process all boxes but only refit/recalculate missing data
        try:
            # Always call applyConvexhull to ensure hists2 is properly generated
            # (especially if it was deleted due to hull_range change)
            if hull_range_changed or box.hist2 is None:
                print(f"Calling applyConvexhull to regenerate hists2...")
                self.projProc.applyConvexhull()
            
            print(f"Calling fitModel...")
            self.projProc.fitModel()
            print(f"Calling getBackgroundSubtractedHistograms...")
            self.projProc.getBackgroundSubtractedHistograms()
            print(f"Calling getPeakInfos...")
            self.projProc.getPeakInfos()
            
            # Retrieve the fit result
            print(f"\n--- Data state AFTER processing ---")
            print(f"  fit_results exists: {box.fit_results is not None}")
            
            if box.fit_results:
                result_dict = box.fit_results
                
                # Save fixed states from paramInfo to fit_results
                for param_name, pinfo in paramInfo.items():
                    if pinfo.get('fixed', False):
                        result_dict[f'{param_name}_fixed'] = True

                # Persist updated ProcessingState to disk cache (same as ProjectionProcessor.process()).
                # This is intentionally done only after a successful refit so cache isn't polluted by
                # failed/incomplete fits during interactive editing.
                try:
                    self.projProc.cacheInfo()
                    print(f"Cached updated ProcessingState for {self.projProc.filename}")
                except Exception as cache_e:
                    # Cache errors should not break refitting; treat as non-fatal.
                    print(f"WARNING: Failed to write cache after refit: {cache_e}")
                
                # Print comparison if we have old result
                if old_result:
                    print(f"\n--- NEW VALUES (after refit) ---")
                    print(f"  common_sigma: {result_dict.get('common_sigma', 'N/A')}")
                    print(f"  error: {result_dict.get('error', 'N/A')}")
                    if use_equal_variance:
                        print(f"  (GMM mode - all sigmas share common_sigma)")
                    else:
                        sigmas = [result_dict.get(f'sigma{i}', None) for i in range(5) if f'sigma{i}' in result_dict]
                        print(f"  individual sigmas (first 5): {sigmas}")
                    
                    print(f"\n--- CHANGES ---")
                    # Compare key parameters
                    params_to_compare = ['common_sigma', 'error']
                    # Add individual sigmas if not GMM
                    if not use_equal_variance:
                        params_to_compare.extend([f'sigma{i}' for i in range(min(5, len([k for k in result_dict.keys() if k.startswith('sigma') and k != 'common_sigma'])))])
                    # Add first 3 positions and amplitudes
                    for i in range(min(3, len([k for k in result_dict.keys() if k.startswith('p_')]))):
                        params_to_compare.append(f'p_{i}')
                        params_to_compare.append(f'amplitude{i}')
                    
                    for key in params_to_compare:
                        if key in result_dict and key in old_result:
                            old_val = old_result[key]
                            new_val = result_dict[key]
                            diff = new_val - old_val
                            percent = (diff / old_val * 100) if old_val != 0 else 0
                            print(f"  {key:15s}: {old_val:10.4f} → {new_val:10.4f}  (Δ={diff:+10.6f}, {percent:+6.2f}%)")
                
                # Debug: print sigma values after fit
                print(f"\nFit complete. Error: {result_dict.get('error', 0):.6f}")
                if use_equal_variance:
                    print(f"  common_sigma: {result_dict.get('common_sigma', 'N/A')}")
                else:
                    sigma_values = [result_dict.get(f'sigma{i}', None) for i in range(20) if f'sigma{i}' in result_dict]
                    print(f"  Individual sigmas: {sigma_values[:5]}...")
                
                print(f"=== Refit completed using unified logic, use_common_sigma = {use_equal_variance} ===\n")
                
                return result_dict
            else:
                print("ERROR: fitModel did not produce results for this box!")
                return None
                
        except Exception as e:
            print(f"ERROR in fitModel: {e}")
            import traceback
            traceback.print_exc()
            return None
    
    def reject(self):
        """
        Close without saving: discard working_params and working_hull_range, exit preview mode
        
        Note: This only discards unsaved edits since the last "Refit & Save".
        Changes that were already saved via "Refit & Save" are kept.
        fit_results and box.hull_range were only modified by successful Refit operations.
        
        The actual cleanup is handled by parent_tab.onParameterEditorClosed() 
        which is triggered by the finished signal.
        """
        # Let parent handle all cleanup via onParameterEditorClosed callback
        super().reject()
    
    def closeEvent(self, event):
        """
        Handle dialog close event (e.g., clicking X button)
        
        The actual cleanup is handled by parent_tab.onParameterEditorClosed() 
        which is triggered by the finished signal when the dialog closes.
        """
        # Let parent handle all cleanup via onParameterEditorClosed callback
        event.accept()
