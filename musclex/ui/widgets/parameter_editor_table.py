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
ParameterEditorTable - Reusable parameter editor table widget

A QTableWidget with columns: Fixed | Parameter | Value | Min | Max
Features:
- Tolerance-based min/max auto-update when value changes
- Fixed checkbox to lock parameters
- Signals for parameter changes (for real-time preview)
"""

from PySide6.QtCore import Qt, Signal
from PySide6.QtWidgets import QTableWidget, QTableWidgetItem, QDoubleSpinBox


class ParameterEditorTable(QTableWidget):
    """
    Reusable parameter editor table with columns:
    Fixed | Parameter | Value | Min | Max
    
    Features:
    - Tolerance-based min/max auto-update on value change
    - Support different parameter types with different tolerance strategies:
      - 'sigma': percentage tolerance (e.g., 100% means ± value * 100%)
      - 'peak': absolute tolerance (e.g., 2.0 means ± 2.0)
      - 'amplitude': no auto bounds (very wide range)
    - Fixed checkbox to lock parameters
    - Signal emitted when any parameter changes
    
    Usage:
        table = ParameterEditorTable()
        table.setToleranceGetter('sigma', lambda: spinbox.value())  # percentage
        table.setToleranceGetter('peak', lambda: spinbox.value())   # absolute
        table.parameterChanged.connect(self.onParameterChanged)
        table.populateParameters({
            'param_name': {
                'val': 1.0, 'min': 0.0, 'max': 2.0, 
                'fixed': False, '_type': 'sigma'
            }
        })
    """
    
    # Signal emitted when any parameter changes
    # Emits dict: {param_name: {'val': float, 'min': float, 'max': float, 'fixed': bool, '_key': str, '_type': str}}
    parameterChanged = Signal(dict)
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self._param_key_map = {}  # Maps display name to internal key
        self._param_type_map = {}  # Maps display name to parameter type
        self._tolerance_getters = {}  # Maps param type to tolerance getter functions
        self._clamp_min_at_zero = True  # Whether to clamp min at 0
        self._setup_table()
        
    def _setup_table(self):
        """Initialize table structure"""
        self.setColumnCount(5)
        self.setHorizontalHeaderLabels(["Fixed", "Parameter", "Value", "Min", "Max"])
        self.horizontalHeader().setStretchLastSection(True)
        self.setColumnWidth(0, 50)
        self.setColumnWidth(1, 200)
        self.setColumnWidth(2, 100)
        self.setColumnWidth(3, 100)
        self.setColumnWidth(4, 100)
        self.itemClicked.connect(self._on_item_clicked)
    
    def setToleranceGetter(self, param_type: str, getter_func):
        """
        Set a tolerance getter for a specific parameter type.
        
        :param param_type: Parameter type (e.g., 'sigma', 'peak', 'amplitude')
        :param getter_func: Callable that returns tolerance value
                           - For 'sigma': returns percentage (e.g., 100.0 for 100%)
                           - For 'peak': returns absolute value (e.g., 2.0)
                           - For 'amplitude': not used (no auto bounds)
        
        Example:
            table.setToleranceGetter('sigma', lambda: spinbox.value())  # percentage
            table.setToleranceGetter('peak', lambda: spinbox.value())   # absolute
        """
        self._tolerance_getters[param_type] = getter_func
    
    def setClampMinAtZero(self, clamp: bool):
        """
        Set whether to clamp min values at 0 when recalculating bounds.
        Default is True.
        
        :param clamp: If True, min will not go below 0
        """
        self._clamp_min_at_zero = clamp
    
    def populateParameters(self, params_dict: dict, order: list = None):
        """
        Populate table from a dictionary.
        
        :param params_dict: Dictionary with structure:
            {
                'display_name': {
                    'val': float,          # Current value
                    'min': float,          # Min bound
                    'max': float,          # Max bound
                    'fixed': bool,         # Whether parameter is fixed
                    'enabled': bool,       # Optional, default True - whether row is editable
                    '_key': str,           # Optional - internal key for mapping
                    '_type': str           # Optional - parameter type ('sigma', 'peak', 'amplitude')
                }
            }
        :param order: Optional list of display names to control row order
        """
        self._param_key_map.clear()
        self._param_type_map.clear()
        
        # Determine order
        if order is None:
            order = list(params_dict.keys())
        
        self.setRowCount(len(order))
        
        for row, display_name in enumerate(order):
            if display_name not in params_dict:
                continue
                
            pdict = params_dict[display_name]
            enabled = pdict.get('enabled', True)
            fixed = pdict.get('fixed', False)
            
            # Store internal key mapping if provided
            if '_key' in pdict:
                self._param_key_map[display_name] = pdict['_key']
            
            # Store parameter type if provided
            if '_type' in pdict:
                self._param_type_map[display_name] = pdict['_type']
            
            # Column 0: Fixed checkbox
            chkItem = QTableWidgetItem()
            chkItem.setFlags(Qt.ItemIsUserCheckable | Qt.ItemIsEnabled)
            chkItem.setCheckState(Qt.Checked if fixed else Qt.Unchecked)
            self.setItem(row, 0, chkItem)
            
            # Column 1: Parameter name (read-only)
            nameItem = QTableWidgetItem(display_name)
            nameItem.setFlags(Qt.ItemIsEnabled)  # Read-only
            self.setItem(row, 1, nameItem)
            
            # Column 2: Value spinbox
            valueSpin = QDoubleSpinBox()
            valueSpin.setDecimals(4)
            valueSpin.setRange(-1e10, 1e10)
            valueSpin.setValue(pdict['val'])
            valueSpin.setEnabled(enabled)
            valueSpin.valueChanged.connect(self._on_value_changed)
            self.setCellWidget(row, 2, valueSpin)
            
            # Column 3: Min spinbox
            minSpin = QDoubleSpinBox()
            minSpin.setDecimals(4)
            minSpin.setRange(-1e10, 1e10)
            minSpin.setValue(pdict['min'])
            minSpin.setEnabled(enabled and not fixed)
            self.setCellWidget(row, 3, minSpin)
            
            # Column 4: Max spinbox
            maxSpin = QDoubleSpinBox()
            maxSpin.setDecimals(4)
            maxSpin.setRange(-1e10, 1e10)
            maxSpin.setValue(pdict['max'])
            maxSpin.setEnabled(enabled and not fixed)
            self.setCellWidget(row, 4, maxSpin)
    
    def _on_item_clicked(self, item):
        """Handle item click - toggle Fixed checkbox enables/disables Min/Max"""
        if item.column() == 0:
            row = item.row()
            minSpin = self.cellWidget(row, 3)
            maxSpin = self.cellWidget(row, 4)
            is_fixed = item.checkState() == Qt.Checked
            
            if minSpin:
                minSpin.setEnabled(not is_fixed)
            if maxSpin:
                maxSpin.setEnabled(not is_fixed)
            
            self._emit_parameter_changed()
    
    def _on_value_changed(self, value):
        """When value changes, recalculate min/max based on parameter type and tolerance"""
        sender = self.sender()
        row = self._find_row_for_widget(sender, col=2)
        if row is not None:
            display_name = self.item(row, 1).text() if self.item(row, 1) else None
            if display_name:
                bmin, bmax = self._calculate_bounds_for_param(display_name, value)
                if bmin is not None and bmax is not None:
                    self._update_bounds_for_row(row, bmin, bmax)
        self._emit_parameter_changed()
    
    def _find_row_for_widget(self, widget, col: int = 2):
        """
        Find row index for a given widget in specified column.
        
        :param widget: The widget to find
        :param col: Column index to search in (default 2 = Value)
        :return: Row index or None
        """
        for row in range(self.rowCount()):
            if self.cellWidget(row, col) is widget:
                return row
        return None
    
    def _calculate_bounds_for_param(self, display_name: str, value: float) -> tuple:
        """
        Calculate min/max bounds based on parameter type and tolerance.
        
        :param display_name: Display name to look up type
        :param value: Current value
        :return: (min, max) tuple, or (None, None) if no calculation needed
        """
        param_type = self._param_type_map.get(display_name, None)
        
        if param_type == 'sigma':
            # Sigma: use percentage tolerance
            if 'sigma' in self._tolerance_getters:
                try:
                    tolerance_pct = float(self._tolerance_getters['sigma']())
                    tolerance_abs = abs(value) * (tolerance_pct / 100.0)
                    bmin = value - tolerance_abs
                    bmax = value + tolerance_abs
                    if self._clamp_min_at_zero:
                        bmin = max(0.0, bmin)
                    return (bmin, bmax)
                except (TypeError, ValueError):
                    pass
            # Fallback: 100% tolerance
            bmin = max(0.0, value * 0.0) if self._clamp_min_at_zero else value * 0.0
            bmax = value * 2.0
            return (bmin, bmax)
        
        elif param_type == 'peak':
            # Peak: use absolute tolerance
            if 'peak' in self._tolerance_getters:
                try:
                    tolerance = float(self._tolerance_getters['peak']())
                    bmin = value - tolerance
                    bmax = value + tolerance
                    if self._clamp_min_at_zero:
                        bmin = max(0.0, bmin)
                    return (bmin, bmax)
                except (TypeError, ValueError):
                    pass
            # Fallback: ±2.0
            tolerance = 2.0
            bmin = value - tolerance
            bmax = value + tolerance
            if self._clamp_min_at_zero:
                bmin = max(0.0, bmin)
            return (bmin, bmax)
        
        elif param_type == 'amplitude':
            # Amplitude: no automatic bounds (very wide range, don't auto-update)
            return (None, None)
        
        else:
            # Unknown type: no auto-update
            return (None, None)
    
    def _update_bounds_for_row(self, row: int, new_min: float, new_max: float):
        """
        Update min/max spinboxes for a row without triggering signals.
        
        :param row: Row index
        :param new_min: New minimum value
        :param new_max: New maximum value
        """
        minSpin = self.cellWidget(row, 3)
        maxSpin = self.cellWidget(row, 4)
        
        if minSpin and maxSpin:
            minSpin.blockSignals(True)
            maxSpin.blockSignals(True)
            minSpin.setValue(new_min)
            maxSpin.setValue(new_max)
            minSpin.blockSignals(False)
            maxSpin.blockSignals(False)
    
    def _emit_parameter_changed(self):
        """Emit signal with current parameters"""
        params = self.getParameters()
        self.parameterChanged.emit(params)
    
    def getParameters(self) -> dict:
        """
        Extract all parameters from table.
        
        :return: Dictionary with structure:
            {
                'display_name': {
                    'val': float,
                    'min': float,
                    'max': float,
                    'fixed': bool,
                    '_key': str (if originally provided),
                    '_type': str (if originally provided)
                }
            }
        """
        params = {}
        for row in range(self.rowCount()):
            name_item = self.item(row, 1)
            fixed_item = self.item(row, 0)
            
            if name_item is None:
                continue
                
            display_name = name_item.text()
            
            result = {
                'fixed': fixed_item.checkState() == Qt.Checked if fixed_item else False,
                'val': self.cellWidget(row, 2).value() if self.cellWidget(row, 2) else 0.0,
                'min': self.cellWidget(row, 3).value() if self.cellWidget(row, 3) else 0.0,
                'max': self.cellWidget(row, 4).value() if self.cellWidget(row, 4) else 0.0,
            }
            
            # Include internal key if available
            if display_name in self._param_key_map:
                result['_key'] = self._param_key_map[display_name]
            
            # Include parameter type if available
            if display_name in self._param_type_map:
                result['_type'] = self._param_type_map[display_name]
            
            params[display_name] = result
        
        return params
    
    def updateParameterBounds(self, display_name: str, new_min: float = None, new_max: float = None):
        """
        Update min/max bounds for a specific parameter.
        If new_min/new_max not provided, recalculate using tolerance.
        
        :param display_name: Display name of the parameter
        :param new_min: New minimum value (optional)
        :param new_max: New maximum value (optional)
        """
        for row in range(self.rowCount()):
            name_item = self.item(row, 1)
            if name_item and name_item.text() == display_name:
                if new_min is None or new_max is None:
                    # Recalculate from current value
                    valueSpin = self.cellWidget(row, 2)
                    if valueSpin:
                        value = valueSpin.value()
                        calc_min, calc_max = self._calculate_bounds_for_param(display_name, value)
                        if calc_min is not None:
                            new_min = calc_min
                        if calc_max is not None:
                            new_max = calc_max
                
                if new_min is not None and new_max is not None:
                    self._update_bounds_for_row(row, new_min, new_max)
                break
    
    def getParameterByKey(self, internal_key: str) -> dict:
        """
        Get parameter info by internal key (if _key was provided during population).
        
        :param internal_key: The internal key to look up
        :return: Parameter dict or None if not found
        """
        params = self.getParameters()
        for display_name, pinfo in params.items():
            if pinfo.get('_key') == internal_key:
                return pinfo
        return None
    
    def updateParameterValue(self, display_name: str, value: float, update_bounds: bool = True):
        """
        Update a specific parameter's value programmatically.
        
        :param display_name: Display name of the parameter
        :param value: New value
        :param update_bounds: Whether to recalculate bounds based on tolerance
        """
        for row in range(self.rowCount()):
            name_item = self.item(row, 1)
            if name_item and name_item.text() == display_name:
                valueSpin = self.cellWidget(row, 2)
                if valueSpin:
                    valueSpin.blockSignals(True)
                    valueSpin.setValue(value)
                    valueSpin.blockSignals(False)
                    
                    if update_bounds:
                        self._reset_bounds_for_row(row, value)
                break
    
    def setParameterEnabled(self, display_name: str, enabled: bool):
        """
        Enable or disable a specific parameter row.
        
        :param display_name: Display name of the parameter
        :param enabled: Whether the parameter should be enabled
        """
        for row in range(self.rowCount()):
            name_item = self.item(row, 1)
            if name_item and name_item.text() == display_name:
                valueSpin = self.cellWidget(row, 2)
                minSpin = self.cellWidget(row, 3)
                maxSpin = self.cellWidget(row, 4)
                fixed_item = self.item(row, 0)
                is_fixed = fixed_item.checkState() == Qt.Checked if fixed_item else False
                
                if valueSpin:
                    valueSpin.setEnabled(enabled)
                if minSpin:
                    minSpin.setEnabled(enabled and not is_fixed)
                if maxSpin:
                    maxSpin.setEnabled(enabled and not is_fixed)
                break
