"""
MuscleX HDF5 -> TIFF GUI

A PySide6 GUI wrapper around the MuscleX HDF5/TIFF conversion workflow.
Pixel normalization is intentionally delegated to
musclex.utils.file_manager.ifHdfReadConvertless so HDF5 uint16/uint32
handling stays consistent with XV and the rest of MuscleX.

Place this file at:
    musclex/utils/hdf5_to_tiffs_gui.py

Run from the repository root with:
    python -m musclex.utils.hdf5_to_tiffs_gui

It can also be run directly:
    python musclex/utils/hdf5_to_tiffs_gui.py
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import fabio
from PySide6.QtCore import QObject, QThread, Signal, Slot
from PySide6.QtWidgets import (
    QApplication,
    QCheckBox,
    QFileDialog,
    QGridLayout,
    QGroupBox,
    QHBoxLayout,
    QLabel,
    QListWidget,
    QMainWindow,
    QMessageBox,
    QPlainTextEdit,
    QProgressBar,
    QPushButton,
    QWidget,
)

# Support both:
#   python -m musclex.utils.hdf5_to_tiffs_gui
# and:
#   python musclex/utils/hdf5_to_tiffs_gui.py
try:
    from musclex.utils.file_manager import ifHdfReadConvertless
except ModuleNotFoundError:
    repo_root = Path(__file__).resolve().parents[2]
    if str(repo_root) not in sys.path:
        sys.path.insert(0, str(repo_root))
    from musclex.utils.file_manager import ifHdfReadConvertless


HDF5_EXTENSIONS = (".h5", ".hdf5", ".nxs")


def _frame_count(path: str) -> int:
    """Return Fabio's frame count for one HDF5 file."""
    with fabio.open(path) as series:
        return int(getattr(series, "nframes", 1) or 1)


def _iter_frames(path: str):
    """Yield Fabio frames without loading the entire stack into memory."""
    with fabio.open(path) as series:
        frames_method = getattr(series, "frames", None)
        if callable(frames_method):
            yield from frames_method()
            return

        nframes = int(getattr(series, "nframes", 1) or 1)
        for index in range(nframes):
            if index == 0:
                yield series
                continue

            getter = getattr(series, "get_frame", None)
            if getter is None:
                getter = getattr(series, "getframe", None)
            if getter is None:
                raise RuntimeError(
                    "This Fabio version does not expose get_frame/getframe for "
                    "multi-frame HDF5 files."
                )
            yield getter(index)


def _write_tiff(path: str, data, header, compress: bool) -> None:
    """
    Write one signed-int32 TIFF frame.

    Uncompressed output uses Fabio, matching MuscleX's image I/O stack.
    Compressed output uses Pillow's TIFF LZW writer.
    """
    if compress:
        from PIL import Image

        Image.fromarray(data).save(path, format="TIFF", compression="tiff_lzw")
    else:
        fabio.tifimage.tifimage(data=data, header=header).write(path)


class ConversionWorker(QObject):
    progress = Signal(int, int, str)
    message = Signal(str)
    finished = Signal(int, int)
    failed = Signal(str)

    def __init__(
        self,
        files: list[str],
        output_dir: str | None,
        same_as_source: bool,
        compress: bool,
    ):
        super().__init__()
        self.files = files
        self.output_dir = output_dir
        self.same_as_source = same_as_source
        self.compress = compress
        self._cancelled = False

    @Slot()
    def cancel(self):
        self._cancelled = True

    @Slot()
    def run(self):
        converted = 0
        errors = 0

        try:
            total_frames = 0
            for file_path in self.files:
                if self._cancelled:
                    self.finished.emit(converted, errors)
                    return
                total_frames += _frame_count(file_path)

            completed = 0

            for file_path in self.files:
                if self._cancelled:
                    break

                source_path = Path(file_path)
                destination = (
                    source_path.parent
                    if self.same_as_source
                    else Path(self.output_dir or source_path.parent)
                )
                destination.mkdir(parents=True, exist_ok=True)

                prefix = source_path.stem

                try:
                    for frame_index, frame in enumerate(_iter_frames(file_path), start=1):
                        if self._cancelled:
                            break

                        # This is the important part: use the corrected central
                        # MuscleX conversion function rather than duplicate dtype
                        # logic in this GUI.
                        data = ifHdfReadConvertless(file_path, frame.data)

                        suffix = "_cmp.tif" if self.compress else ".tif"
                        out_name = f"{prefix}_{frame_index:04d}{suffix}"
                        out_path = destination / out_name

                        header = {}
                        try:
                            header = frame.getheader()
                        except Exception:
                            try:
                                header = frame.header
                            except Exception:
                                pass

                        _write_tiff(
                            str(out_path),
                            data,
                            header,
                            self.compress,
                        )

                        completed += 1
                        converted += 1
                        self.progress.emit(
                            completed,
                            total_frames,
                            f"{source_path.name}  frame {frame_index} -> {out_name}",
                        )

                except Exception as exc:
                    errors += 1
                    self.message.emit(f"ERROR: {source_path.name}: {exc}")

            self.finished.emit(converted, errors)

        except Exception as exc:
            self.failed.emit(str(exc))


class H5ToTiffWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("MuscleX HDF5 to TIFF Converter")
        self.resize(800, 620)

        self._thread = None
        self._worker = None

        root = QWidget()
        self.setCentralWidget(root)
        layout = QGridLayout(root)

        # Input files
        input_group = QGroupBox("HDF5 input files")
        input_layout = QGridLayout(input_group)

        self.file_list = QListWidget()
        self.add_button = QPushButton("Add HDF5 Files...")
        self.remove_button = QPushButton("Remove Selected")
        self.clear_button = QPushButton("Clear")

        input_layout.addWidget(self.file_list, 0, 0, 1, 3)
        input_layout.addWidget(self.add_button, 1, 0)
        input_layout.addWidget(self.remove_button, 1, 1)
        input_layout.addWidget(self.clear_button, 1, 2)

        # Output settings
        output_group = QGroupBox("Output")
        output_layout = QGridLayout(output_group)

        self.same_folder_checkbox = QCheckBox("Write TIFF files beside each HDF5 file")
        self.same_folder_checkbox.setChecked(True)

        self.output_label = QLabel("No output folder selected")
        self.output_label.setTextInteractionFlags(self.output_label.textInteractionFlags())
        self.choose_output_button = QPushButton("Choose Output Folder...")
        self.choose_output_button.setEnabled(False)

        self.compress_checkbox = QCheckBox("LZW-compressed TIFF (_cmp.tif)")
        self.compress_checkbox.setChecked(False)

        output_layout.addWidget(self.same_folder_checkbox, 0, 0, 1, 2)
        output_layout.addWidget(self.output_label, 1, 0)
        output_layout.addWidget(self.choose_output_button, 1, 1)
        output_layout.addWidget(self.compress_checkbox, 2, 0, 1, 2)

        # Conversion status
        status_group = QGroupBox("Conversion")
        status_layout = QGridLayout(status_group)

        self.progress_bar = QProgressBar()
        self.progress_bar.setMinimum(0)
        self.progress_bar.setMaximum(100)
        self.progress_label = QLabel("Ready")

        self.convert_button = QPushButton("Convert")
        self.cancel_button = QPushButton("Cancel")
        self.cancel_button.setEnabled(False)

        self.log = QPlainTextEdit()
        self.log.setReadOnly(True)

        status_layout.addWidget(self.progress_label, 0, 0, 1, 2)
        status_layout.addWidget(self.progress_bar, 1, 0, 1, 2)
        status_layout.addWidget(self.convert_button, 2, 0)
        status_layout.addWidget(self.cancel_button, 2, 1)
        status_layout.addWidget(self.log, 3, 0, 1, 2)

        layout.addWidget(input_group, 0, 0)
        layout.addWidget(output_group, 1, 0)
        layout.addWidget(status_group, 2, 0)

        self.output_dir = None

        self.add_button.clicked.connect(self.add_files)
        self.remove_button.clicked.connect(self.remove_selected)
        self.clear_button.clicked.connect(self.file_list.clear)
        self.same_folder_checkbox.toggled.connect(self._same_folder_changed)
        self.choose_output_button.clicked.connect(self.choose_output_folder)
        self.convert_button.clicked.connect(self.start_conversion)
        self.cancel_button.clicked.connect(self.cancel_conversion)

    def add_files(self):
        paths, _ = QFileDialog.getOpenFileNames(
            self,
            "Select HDF5 files",
            "",
            "HDF5 files (*.h5 *.hdf5 *.nxs);;All files (*)",
        )
        existing = {
            self.file_list.item(i).text()
            for i in range(self.file_list.count())
        }

        for path in paths:
            if path not in existing:
                self.file_list.addItem(path)
                existing.add(path)

    def remove_selected(self):
        for item in self.file_list.selectedItems():
            self.file_list.takeItem(self.file_list.row(item))

    def _same_folder_changed(self, checked: bool):
        self.choose_output_button.setEnabled(not checked)
        if checked:
            self.output_label.setText("Output: same folder as each HDF5 file")
        elif self.output_dir:
            self.output_label.setText(self.output_dir)
        else:
            self.output_label.setText("No output folder selected")

    def choose_output_folder(self):
        directory = QFileDialog.getExistingDirectory(
            self,
            "Choose TIFF output folder",
            self.output_dir or "",
        )
        if directory:
            self.output_dir = directory
            self.output_label.setText(directory)

    def start_conversion(self):
        files = [
            self.file_list.item(i).text()
            for i in range(self.file_list.count())
        ]
        if not files:
            QMessageBox.information(self, "No input", "Add at least one HDF5 file.")
            return

        same_as_source = self.same_folder_checkbox.isChecked()
        if not same_as_source and not self.output_dir:
            QMessageBox.information(
                self,
                "No output folder",
                "Choose an output folder or enable same-folder output.",
            )
            return

        invalid = [
            path
            for path in files
            if Path(path).suffix.lower() not in HDF5_EXTENSIONS
        ]
        if invalid:
            QMessageBox.warning(
                self,
                "Invalid input",
                "Only .h5, .hdf5, and .nxs files can be converted.",
            )
            return

        self.log.clear()
        self.progress_bar.setValue(0)
        self.progress_label.setText("Starting...")
        self._set_running(True)

        self._thread = QThread(self)
        self._worker = ConversionWorker(
            files=files,
            output_dir=self.output_dir,
            same_as_source=same_as_source,
            compress=self.compress_checkbox.isChecked(),
        )
        self._worker.moveToThread(self._thread)

        self._thread.started.connect(self._worker.run)
        self._worker.progress.connect(self._on_progress)
        self._worker.message.connect(self.log.appendPlainText)
        self._worker.finished.connect(self._on_finished)
        self._worker.failed.connect(self._on_failed)

        self._worker.finished.connect(self._thread.quit)
        self._worker.failed.connect(self._thread.quit)
        self._thread.finished.connect(self._cleanup_thread)

        self._thread.start()

    def cancel_conversion(self):
        if self._worker is not None:
            self._worker.cancel()
            self.cancel_button.setEnabled(False)
            self.progress_label.setText("Cancelling after current frame...")

    @Slot(int, int, str)
    def _on_progress(self, completed: int, total: int, message: str):
        if total > 0:
            self.progress_bar.setValue(int(completed * 100 / total))
        self.progress_label.setText(f"{completed} / {total} frames")
        self.log.appendPlainText(message)

    @Slot(int, int)
    def _on_finished(self, converted: int, errors: int):
        if self._worker is not None and self._worker._cancelled:
            self.progress_label.setText(
                f"Cancelled. Converted {converted} frame(s); {errors} file error(s)."
            )
        else:
            self.progress_bar.setValue(100 if errors == 0 else self.progress_bar.value())
            self.progress_label.setText(
                f"Completed. Converted {converted} frame(s); {errors} file error(s)."
            )
        self._set_running(False)

    @Slot(str)
    def _on_failed(self, error: str):
        self.log.appendPlainText(f"FATAL ERROR: {error}")
        self.progress_label.setText("Conversion failed")
        self._set_running(False)
        QMessageBox.critical(self, "Conversion failed", error)

    def _cleanup_thread(self):
        if self._worker is not None:
            self._worker.deleteLater()
        if self._thread is not None:
            self._thread.deleteLater()
        self._worker = None
        self._thread = None

    def _set_running(self, running: bool):
        self.add_button.setEnabled(not running)
        self.remove_button.setEnabled(not running)
        self.clear_button.setEnabled(not running)
        self.same_folder_checkbox.setEnabled(not running)
        self.choose_output_button.setEnabled(
            not running and not self.same_folder_checkbox.isChecked()
        )
        self.compress_checkbox.setEnabled(not running)
        self.convert_button.setEnabled(not running)
        self.cancel_button.setEnabled(running)


def main():
    app = QApplication.instance() or QApplication(sys.argv)
    app.setStyle("Fusion")

    window = H5ToTiffWindow()
    window.show()

    return app.exec()


if __name__ == "__main__":
    raise SystemExit(main())
