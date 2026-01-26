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

"""
EditPeakDetailsDialog - Meridional Peak Parameter Editor

A dialog for editing meridional peak fitting parameters with:
- Table-style UI (Fixed | Parameter | Value | Min | Max)
- Real-time graph preview
- Integrated Refit & Save functionality
- Tolerance-based min/max auto-update:
  - Sigma: percentage tolerance (from parent tab's sigmaToleranceSpinBox)
  - Amplitude: no auto bounds (wide range)

Consistent with GMMParameterEditorDialog style.
"""

from .pyqt_utils import *
from .widgets.parameter_editor_table import ParameterEditorTable


class EditPeakDetailsDialog(QDialog):
    """
    Meridional Peak parameter editor dialog.
    
    Edit and refit meridional peak parameters:
    - bg_sigma, center_sigma1, center_sigma2 (Sigma - percentage tolerance)
    - bg_amplitude, center_amplitude1, center_amplitude2 (Amplitude - no auto bounds)
    
    Features:
    - Table-style UI consistent with GMMParameterEditorDialog
    - Real-time preview on parameter changes
    - Integrated Refit & Save button
    - Tolerance-based min/max bound calculation:
      - Sigma uses percentage tolerance from parent tab
      - Amplitude uses very wide range (no auto bounds)
    """
    
    # Parameter configuration: (internal_key, display_name, color_hint, param_type)
    # param_type: 'sigma' or 'amplitude'
    PARAMS_CONFIG = [
        ('bg_sigma', 'Background Sigma', 'blue', 'sigma'),
        ('bg_amplitude', 'Background Amplitude', 'blue', 'amplitude'),
        ('center_sigma1', 'Meridian Bg Sigma', 'yellow', 'sigma'),
        ('center_amplitude1', 'Meridian Amplitude', 'yellow', 'amplitude'),
        ('center_sigma2', 'Meridian Sigma', 'red', 'sigma'),
        ('center_amplitude2', 'Meridian Amplitude', 'red', 'amplitude'),
    ]
    
    def __init__(self, parent_tab, box_name):
        """
        Initialize the dialog.
        
        :param parent_tab: The ProjectionBoxTab instance
        :param box_name: Name of the box (e.g., 'left' or 'right')
        """
        super().__init__(parent_tab)
        self.parent_tab = parent_tab
        self.box_name = box_name
        self.projProc = parent_tab.parent.projProc
        
        # Get box and validate
        box = self.projProc.boxes.get(self.box_name)
        if box is None or box.fit_results is None:
            raise ValueError(f"No fit results for box '{box_name}'")
        
        # Working copy for preview mode
        self.working_params = dict(box.fit_results)
        
        # Enable preview mode on parent tab
        self.parent_tab.preview_params = self.working_params
        
        self.setWindowTitle(f"Edit Meridional Peak - {box_name}")
        self.resize(700, 450)
        self.setWindowFlags(Qt.Window | Qt.WindowCloseButtonHint | Qt.WindowStaysOnTopHint)
        
        self._init_ui()
        self._populate_parameters()
    
    def _init_ui(self):
        """Initialize the UI layout."""
        mainLayout = QVBoxLayout(self)
        
        # Title
        titleLabel = QLabel(f"<h2>Meridional Peak Parameters - {self.box_name}</h2>")
        mainLayout.addWidget(titleLabel)
        
        # Description
        descLabel = QLabel(
            "Edit meridional peak fitting parameters. Changes are previewed in real-time.\n"
            "Sigma parameters use percentage tolerance from main tab.\n"
            "Click 'Refit & Save' to apply and save changes."
        )
        descLabel.setWordWrap(True)
        descLabel.setStyleSheet("color: gray; margin-bottom: 10px;")
        mainLayout.addWidget(descLabel)
        
        # Parameter table - configure tolerance getters
        self.paramTable = ParameterEditorTable()
        
        # Set tolerance getters for different parameter types
        # Sigma: percentage tolerance from parent tab's spinbox
        self.paramTable.setToleranceGetter(
            'sigma',
            lambda: self.parent_tab.sigmaToleranceSpinBox.value()
        )
        # Note: amplitude type has no tolerance getter (no auto bounds)
        
        self.paramTable.setClampMinAtZero(False)  # Allow negative for amplitudes
        self.paramTable.parameterChanged.connect(self._on_parameter_changed)
        mainLayout.addWidget(self.paramTable)
        
        # Buttons
        buttonLayout = QHBoxLayout()
        
        self.refitBtn = QPushButton("Refit && Save")
        self.refitBtn.clicked.connect(self._on_refit)
        self.refitBtn.setAutoDefault(False)
        self.refitBtn.setDefault(False)
        self.refitBtn.setToolTip(
            "Re-fit with current parameters and save changes.\n"
            "You can continue editing after saving."
        )
        
        self.cancelBtn = QPushButton("Close")
        self.cancelBtn.clicked.connect(self.reject)
        self.cancelBtn.setAutoDefault(False)
        self.cancelBtn.setDefault(False)
        self.cancelBtn.setToolTip(
            "Close dialog and discard unsaved edits.\n"
            "(Already saved changes from 'Refit & Save' will be kept)"
        )
        
        buttonLayout.addWidget(self.refitBtn)
        buttonLayout.addWidget(self.cancelBtn)
        mainLayout.addLayout(buttonLayout)
    
    def _get_sigma_tolerance_percentage(self) -> float:
        """Get sigma tolerance percentage from parent tab's spinbox."""
        try:
            return float(self.parent_tab.sigmaToleranceSpinBox.value())
        except Exception:
            return 100.0  # Default 100%
    
    def _populate_parameters(self):
        """
        Populate the parameter table from fit_results and box.param_bounds.
        
        Logic:
        1. Read values from fit_results
        2. Read bounds from box.param_bounds if available
        3. If no stored bounds, calculate using tolerance
        """
        fit_result = self.working_params
        
        # Get stored bounds from box (persisted from previous edits)
        box = self.projProc.boxes.get(self.box_name)
        box_bounds = {}
        if box and hasattr(box, 'param_bounds') and isinstance(box.param_bounds, dict):
            box_bounds = box.param_bounds
        
        # Get tolerance percentage for sigma params
        sigma_tolerance_pct = self._get_sigma_tolerance_percentage()
        
        params_dict = {}
        order = []
        
        for param_key, display_name, color, param_type in self.PARAMS_CONFIG:
            val = fit_result.get(param_key, 0.0)
            fixed = fit_result.get(f'{param_key}_lock', False)
            
            # Check if bounds are already stored
            stored_bounds = box_bounds.get(param_key, {})
            if not isinstance(stored_bounds, dict):
                stored_bounds = {}
            
            # Use stored bounds if available, otherwise calculate from tolerance
            if 'min' in stored_bounds and 'max' in stored_bounds:
                # Use persisted bounds
                bmin = stored_bounds['min']
                bmax = stored_bounds['max']
            else:
                # Calculate initial bounds based on parameter type
                if param_type == 'sigma':
                    # Sigma: use percentage tolerance
                    tolerance_abs = abs(val) * (sigma_tolerance_pct / 100.0)
                    bmin = max(0.0, val - tolerance_abs)
                    bmax = val + tolerance_abs
                elif param_type == 'amplitude':
                    # Amplitude: wide range, no strict limits
                    if val == 0:
                        bmin, bmax = -1e6, 1e6
                    else:
                        scale = abs(val) * 5
                        bmin, bmax = -scale, scale
                else:
                    # Fallback: fixed tolerance
                    bmin, bmax = val - 10, val + 10
            
            # Create display name with color hint
            full_display_name = f"{display_name} ({color})"
            order.append(full_display_name)
            
            params_dict[full_display_name] = {
                'val': val,
                'min': bmin,
                'max': bmax,
                'fixed': fixed,
                'enabled': True,
                '_key': param_key,
                '_type': param_type
            }
        
        self.paramTable.populateParameters(params_dict, order)
    
    def _on_parameter_changed(self, params: dict):
        """
        Handle real-time parameter changes for preview.
        
        :param params: Dictionary from ParameterEditorTable.getParameters()
        """
        # Update working_params with new values
        for display_name, pinfo in params.items():
            internal_key = pinfo.get('_key')
            if internal_key:
                self.working_params[internal_key] = pinfo['val']
                self.working_params[f'{internal_key}_lock'] = pinfo['fixed']
        
        # Trigger preview update
        self.parent_tab.need_update = True
        self.parent_tab.updateUI()
    
    def _on_refit(self):
        """Refit with current parameters and save."""
        QApplication.setOverrideCursor(Qt.WaitCursor)
        
        try:
            # Get parameters from table
            params = self.paramTable.getParameters()
            
            # Build main_peak_info dictionary
            new_info = {}
            for display_name, pinfo in params.items():
                internal_key = pinfo.get('_key')
                if internal_key:
                    new_info[internal_key] = pinfo['val']
                    new_info[f'{internal_key}_lock'] = pinfo['fixed']
            
            # Save bounds to box.param_bounds (for persistence)
            box = self.projProc.boxes[self.box_name]
            if not hasattr(box, 'param_bounds') or box.param_bounds is None:
                box.param_bounds = {}
            
            for display_name, pinfo in params.items():
                internal_key = pinfo.get('_key')
                if internal_key:
                    box.param_bounds[internal_key] = {
                        'min': pinfo['min'],
                        'max': pinfo['max']
                    }
            
            # Clear fit_results to force refit
            box.fit_results = None
            
            # Update state with new main_peak_info
            self.projProc.state.main_peak_info[self.box_name] = new_info
            
            # Trigger reprocessing pipeline
            self.projProc.fitModel()
            self.projProc.getBackgroundSubtractedHistograms()
            self.projProc.getPeakInfos()
            
            # Update working params from new fit_results
            if box.fit_results:
                self.working_params = dict(box.fit_results)
                self.parent_tab.preview_params = self.working_params
                self._populate_parameters()
                
                # Persist to disk cache
                try:
                    self.projProc.cacheInfo()
                except Exception:
                    pass  # Cache errors are non-fatal
            
            # Update parent UI
            self.parent_tab.need_update = True
            self.parent_tab.updateUI()
            
            # Force canvas redraw
            if hasattr(self.parent_tab, 'graphCanvas1'):
                self.parent_tab.graphCanvas1.draw()
            if hasattr(self.parent_tab, 'graphCanvas2'):
                self.parent_tab.graphCanvas2.draw()
            
            QApplication.restoreOverrideCursor()
            
            # Show success message
            QMessageBox.information(
                self,
                "Refit & Save Complete",
                "✓ Parameters saved successfully!\n\n"
                "You can:\n"
                "• Continue editing and save again\n"
                "• Click 'Close' to exit (unsaved edits will be discarded)"
            )
            
        except Exception as e:
            QApplication.restoreOverrideCursor()
            QMessageBox.critical(self, "Refit Error", str(e))
            import traceback
            traceback.print_exc()
    
    def reject(self):
        """Close dialog and exit preview mode."""
        self.parent_tab.preview_params = None
        self.parent_tab.need_update = True
        self.parent_tab.updateUI()
        super().reject()
    
    def closeEvent(self, event):
        """Handle dialog close event."""
        self.parent_tab.preview_params = None
        self.parent_tab.need_update = True
        self.parent_tab.updateUI()
        event.accept()
