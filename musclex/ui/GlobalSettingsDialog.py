import matplotlib.patches as mpatches
from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QDialogButtonBox, QSplitter, QWidget
)
from PySide6.QtCore import Signal, Qt

from musclex.ui.widgets.image_viewer_widget import ImageViewerWidget
from musclex.ui.widgets.center_settings_widget import CenterSettingsWidget
from musclex.ui.widgets.rotation_settings_widget import RotationSettingsWidget
from musclex.ui.tools.chords_center_tool import ChordsCenterTool
from musclex.ui.tools.perpendiculars_center_tool import PerpendicularsCenterTool
from musclex.ui.tools.rotation_tool import RotationTool
from musclex.ui.tools.center_rotate_tool import CenterRotateTool


class GlobalSettingsDialog(QDialog):
    """Dialog for setting the global base center and rotation for the experiment."""

    globalBaseChanged = Signal()

    def __init__(self, parent, workspace, image_data):
        super().__init__(parent)
        self.setWindowTitle("Global Base Settings")
        self._workspace = workspace
        self._image_data = image_data

        existing = workspace.settings_manager.get_global_base()
        if existing.get('center'):
            self._pending_center = tuple(existing['center'])
        else:
            self._pending_center = image_data.center

        if existing.get('rotation') is not None:
            self._pending_rotation = existing['rotation']
        else:
            self._pending_rotation = image_data.rotation or 0.0

        self._build_ui()
        self._register_tools()
        self._connect_signals()

        self.viewer.display_image(image_data.img)
        self._update_labels()
        self._redraw_overlays()
        self.resize(1000, 700)

    def _build_ui(self):
        layout = QVBoxLayout(self)

        self._base_label = QLabel()
        self._base_label.setStyleSheet("font-weight: bold;")
        layout.addWidget(self._base_label)

        splitter = QSplitter(Qt.Horizontal)

        self.viewer = ImageViewerWidget(
            show_display_panel=True,
            show_double_zoom=False,
        )
        splitter.addWidget(self.viewer)

        right = QWidget()
        right_layout = QVBoxLayout(right)
        right_layout.setContentsMargins(4, 4, 4, 4)

        self._center_widget = CenterSettingsWidget(parent=None)
        self._center_widget.calibrationButton.setVisible(True)
        self._center_widget.applyCenterBtn.setVisible(False)
        self._center_widget.restoreAutoCenterBtn.setVisible(False)
        self._center_widget.modeLabel.setVisible(False)
        right_layout.addWidget(self._center_widget)

        self._rotation_widget = RotationSettingsWidget(parent=None)
        self._rotation_widget.setAutoOrientationBtn.setVisible(False)
        self._rotation_widget.applyRotationBtn.setVisible(False)
        self._rotation_widget.restoreAutoRotationBtn.setVisible(False)
        self._rotation_widget.modeLabel.setVisible(False)
        right_layout.addWidget(self._rotation_widget)

        right_layout.addStretch()
        splitter.addWidget(right)
        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 0)
        splitter.setSizes([600, 400])
        layout.addWidget(splitter)

        if self.viewer.display_panel:
            right_layout.insertWidget(0, self.viewer.display_panel)

        btn_box = QDialogButtonBox(QDialogButtonBox.Apply | QDialogButtonBox.Cancel)
        btn_box.button(QDialogButtonBox.Apply).clicked.connect(self._on_apply)
        btn_box.rejected.connect(self.reject)
        layout.addWidget(btn_box)

    def _register_tools(self):
        tm = self.viewer.tool_manager
        tm.register_tool('chords', ChordsCenterTool)
        tm.register_tool('perpendiculars', PerpendicularsCenterTool)
        tm.register_tool('rotation', RotationTool, self._get_pending_center)
        tm.register_tool('center_rotate', CenterRotateTool)

    def _connect_signals(self):
        self.viewer.toolCompleted.connect(self._on_tool_completed)

        self._center_widget.setCentByChords.clicked.connect(
            lambda checked: self._toggle_tool('chords', checked)
        )
        self._center_widget.setCentByPerp.clicked.connect(
            lambda checked: self._toggle_tool('perpendiculars', checked)
        )
        self._center_widget.setCenterRotationButton.clicked.connect(
            lambda checked: self._toggle_tool('center_rotate', checked)
        )
        self._center_widget.setCentBtn.clicked.connect(self._on_set_center_manually)
        self._center_widget.calibrationButton.clicked.connect(self._on_calibration_clicked)

        self._rotation_widget.setRotationButton.clicked.connect(
            lambda checked: self._toggle_tool('rotation', checked)
        )
        self._rotation_widget.setAngleBtn.clicked.connect(self._on_set_angle_manually)

    # ----------------------------------------------------------------
    # Tool management
    # ----------------------------------------------------------------

    def _toggle_tool(self, name, checked):
        if checked:
            self.viewer.tool_manager.activate_tool(name)
        else:
            result = self.viewer.tool_manager.deactivate_tool(name)
            if result:
                self._dispatch_result(name, result)

    def _on_tool_completed(self, tool_name, result):
        self._dispatch_result(tool_name, result)

    def _dispatch_result(self, tool_name, result):
        if tool_name in ('chords', 'perpendiculars'):
            self._pending_center = (float(result[0]), float(result[1]))
            self._center_widget.setCentByChords.setChecked(False)
            self._center_widget.setCentByPerp.setChecked(False)
        elif tool_name == 'rotation':
            self._pending_rotation = (self._pending_rotation or 0.0) + float(result)
            self._rotation_widget.setRotationButton.setChecked(False)
        elif tool_name == 'center_rotate':
            self._pending_center = (float(result['center'][0]), float(result['center'][1]))
            self._pending_rotation = (self._pending_rotation or 0.0) + float(result['angle'])
            self._center_widget.setCenterRotationButton.setChecked(False)
        self._update_labels()
        self._redraw_overlays()

    # ----------------------------------------------------------------
    # Manual dialogs
    # ----------------------------------------------------------------

    def _on_calibration_clicked(self):
        from musclex.CalibrationSettings import CalibrationSettings
        settings_dir = self._workspace.settings_manager.settings_dir
        cache = self._workspace._load_calibration_cache()
        cal_dialog = CalibrationSettings(
            str(settings_dir),
            center=self._pending_center,
            quadrant_folded=self._image_data.is_quadrant_folded,
            initial_settings=cache,
        )
        cal_dialog.recalculate = False
        if cal_dialog.exec_() == 1:
            cal_settings = cal_dialog.getValues()
            if cal_settings and 'center' in cal_settings:
                self._pending_center = tuple(cal_settings['center'])
                self._update_labels()
                self._redraw_overlays()

    def _on_set_center_manually(self):
        if self._pending_center is None:
            return
        from musclex.ui.SetCentDialog import SetCentDialog
        img = self._image_data.img
        display = self.viewer.get_display_options()
        dlg = SetCentDialog(
            self, img, self._pending_center,
            isLogScale=display['log_scale'],
            vmin=display['vmin'], vmax=display['vmax'],
        )
        if dlg.exec():
            self._pending_center = dlg.center
            self._update_labels()
            self._redraw_overlays()

    def _on_set_angle_manually(self):
        from musclex.ui.SetAngleDialog import SetAngleDialog
        img = self.viewer.get_current_image_data()
        if img is None:
            return
        h, w = img.shape[:2]
        display_center = (w // 2, h // 2)
        display = self.viewer.get_display_options()
        dlg = SetAngleDialog(
            self, img, display_center, self._pending_rotation or 0.0,
            isLogScale=display['log_scale'],
            vmin=display['vmin'], vmax=display['vmax'],
        )
        if dlg.exec():
            self._pending_rotation = (self._pending_rotation or 0.0) + dlg.get_angle()
            self._update_labels()

    # ----------------------------------------------------------------
    # Helpers
    # ----------------------------------------------------------------

    def _get_pending_center(self):
        return self._pending_center

    def _update_labels(self):
        self._base_label.setText(
            f"Base Image: {self._image_data.img_name}"
        )
        if self._pending_center:
            self._center_widget.update_current_center(self._pending_center)
            self._center_widget.update_mode_indicator(is_manual=True)
        if self._pending_rotation is not None:
            self._rotation_widget.update_rotation_display(self._pending_rotation)
            self._rotation_widget.update_mode_indicator(is_manual=True)

    def _redraw_overlays(self):
        ax = self.viewer.axes
        for patch in list(ax.patches):
            patch.remove()
        if self._pending_center:
            cx, cy = self._pending_center
            circle = mpatches.Circle(
                (cx, cy), 10, color='g', fill=False, linewidth=1.5
            )
            ax.add_patch(circle)
        self.viewer.canvas.draw_idle()

    # ----------------------------------------------------------------
    # Apply / Cancel
    # ----------------------------------------------------------------

    def _on_apply(self):
        sm = self._workspace.settings_manager
        sm.set_global_base(
            self._pending_center,
            self._pending_rotation,
            self._image_data.img_name,
        )
        sm.save_global_base()
        self.globalBaseChanged.emit()
        self.accept()
