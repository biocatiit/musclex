"""Regenerate GUI summary baselines using the actual QF and PT GUI workflows."""

import argparse
import csv
import importlib
import os
import shutil
import subprocess
import sys
import tempfile
import time
from datetime import datetime
from pathlib import Path

from PySide6.QtCore import QTimer
from PySide6.QtWidgets import (
    QApplication,
    QDialog,
    QFileDialog,
    QMessageBox,
    QPushButton,
)


REPO_ROOT = Path(__file__).resolve().parents[2]
IMAGE_ROOT = REPO_ROOT / "musclex" / "tests" / "testImages"
IMAGE_EXTENSIONS = {
    ".adsc",
    ".cbf",
    ".edf",
    ".fit2d",
    ".mar345",
    ".marccd",
    ".pilatus",
    ".tif",
    ".tiff",
    ".smv",
}
DATASETS = {
    "qf": (
        "EIGERimages",
        "MARimages",
        "PILATUSimages",
    ),
    "pt": (
        "EIGER_PT_Convex_Hull_Vertical",
        "MAR_PT_Convex_Hull_Vertical",
        "PT_FittingGaussians_Horizontal",
    ),
}


class BaselineRun:
    def __init__(self, app, module, dataset_dir, staging_dir, timeout):
        self.app = app
        self.module = module
        self.dataset_dir = dataset_dir
        self.staging_dir = staging_dir
        self.timeout = timeout
        self.window = None
        self.error = None
        self.dialog_errors = []
        self.started = False
        self.processing_requested = False
        self.qf_background_applied = False
        self.qf_job_settings = {}
        self.qf_expected_summary = {}
        self.original_pt_template_update = None
        self.pt_gui_class = None
        self.worker_refs = []
        self.start_time = time.monotonic()
        self.timer = QTimer()
        self.timer.setInterval(100)
        self.timer.timeout.connect(self._poll)

    def run(self):
        images = sorted(
            path
            for path in self.dataset_dir.iterdir()
            if path.is_file() and path.suffix.lower() in IMAGE_EXTENSIONS
        )
        if not images:
            raise RuntimeError(f"No supported image files in {self.dataset_dir}")

        settings_path = self.dataset_dir / f"{self.module}settings.json"
        if not settings_path.is_file():
            raise FileNotFoundError(f"Settings file not found: {settings_path}")
        input_settings_dir = self.dataset_dir / "settings"
        if input_settings_dir.is_dir():
            shutil.copytree(input_settings_dir, self.staging_dir / "settings")

        from ..utils.directory_context import DirectoryContext
        from ..ui.widgets import output_dir_dialog

        original_resolver = output_dir_dialog.resolve_output_directory
        from ..ui.widgets.image_navigator_widget import ImageNavigatorWidget

        original_browse_file = ImageNavigatorWidget.browse_file

        def resolve_to_staging(input_dir, parent=None):
            if os.path.realpath(input_dir) == os.path.realpath(self.dataset_dir):
                return DirectoryContext(str(self.dataset_dir), str(self.staging_dir))
            return original_resolver(input_dir, parent=parent)

        def load_dataset_from_browse(navigator):
            navigator.load_from_file(str(images[0]), start_background_scan=False)
            return True

        self.timer.start()
        output_dir_dialog.resolve_output_directory = resolve_to_staging
        ImageNavigatorWidget.browse_file = load_dataset_from_browse
        try:
            if self.module == "qf":
                from ..ui.QuadrantFoldingGUI import QuadrantFoldingGUI

                self.window = QuadrantFoldingGUI()
                self._route_qf_batch_output_to_staging()
            else:
                from ..ui.ProjectionTracesGUI import ProjectionTracesGUI

                self.pt_gui_class = ProjectionTracesGUI
                self.original_pt_template_update = (
                    ProjectionTracesGUI._update_folder_cache_from_results
                )

                def preserve_loaded_template(_window):
                    print("DEBUG: suppressed PT template update")
                    return None

                ProjectionTracesGUI._update_folder_cache_from_results = (
                    preserve_loaded_template
                )
                self.window = ProjectionTracesGUI()
                self._retain_projection_workers()
                self.window.workspace.load_from_file(
                    str(images[0]), start_background_scan=False
                )
        except Exception:
            self.timer.stop()
            raise
        finally:
            output_dir_dialog.resolve_output_directory = original_resolver
            ImageNavigatorWidget.browse_file = original_browse_file
        self.window.show()
        QTimer.singleShot(0, lambda: self._load_settings(settings_path))
        self.app.exec()

        if self.module == "pt" and not self.window.threadPool.waitForDone(5000):
            raise RuntimeError("Projection Traces workers did not shut down cleanly")
        self._restore_pt_template_update()
        if self.window is not None:
            self.window.close()
        if self.error:
            raise RuntimeError(self.error)
        if self.dialog_errors:
            raise RuntimeError("; ".join(self.dialog_errors))

        result_name = f"{self.module}_results"
        summary = self.staging_dir / result_name / "summary.csv"
        self._verify_summary(summary, images)
        if self.module == "qf":
            self._verify_qf_summary(summary)
        return self.staging_dir / result_name

    def _route_qf_batch_output_to_staging(self):
        build_job_args = self.window._build_qf_job_args
        record_task_success = self.window._record_task_success

        def build_staged_job_args(*args, **kwargs):
            job_args = build_job_args(*args, **kwargs)
            if job_args is not None:
                if not job_args.get("flags", {}).get("fold_bg_image"):
                    raise RuntimeError(
                        "QF background subtraction was not enabled after applying settings"
                    )
                job_args["output_dir"] = str(self.staging_dir)
                self.qf_job_settings[job_args["job_index"]] = {
                    key: job_args[key]
                    for key in (
                        "apply_blank",
                        "apply_mask",
                        "blank_weight",
                    )
                }
            return job_args

        def record_processed_result(task, result):
            if isinstance(result, dict) and isinstance(result.get("info"), dict):
                processing_flags = dict(result.get("processing_flags") or {})
                runtime_settings = self.qf_job_settings.get(task.job_index)
                if runtime_settings is not None:
                    processing_flags.update(
                        {
                            "blank_enabled": runtime_settings["apply_blank"],
                            "mask_enabled": runtime_settings["apply_mask"],
                            "blank_weight": runtime_settings["blank_weight"],
                        }
                    )
                for key in ("n_peaks", "peak_width"):
                    if key in result["info"]:
                        processing_flags[key] = result["info"][key]
                result = {**result, "processing_flags": processing_flags}
                if runtime_settings is not None:
                    self.qf_expected_summary[task.filename] = {
                        "blank_enabled": runtime_settings["apply_blank"],
                        "mask_enabled": runtime_settings["apply_mask"],
                        "blank_weight": runtime_settings["blank_weight"],
                        **{
                            key: result["info"][key]
                            for key in ("n_peaks", "peak_width")
                            if key in result["info"]
                        },
                    }
            record_task_success(task, result)

        self.window._build_qf_job_args = build_staged_job_args
        self.window._record_task_success = record_processed_result

    def _retain_projection_workers(self):
        start_next_task = self.window.startNextTask

        def retain_workers():
            start_next_task()
            worker = self.window.currentTask
            if worker is not None and all(
                worker is not existing for existing in self.worker_refs
            ):
                self.worker_refs.append(worker)

        self.window.startNextTask = retain_workers

    def _load_settings(self, settings_path):
        if self.error or self.window is None:
            return
        original_get_file = None
        original_get_open_file_name = None
        try:
            if self.module == "qf":
                qf_module = importlib.import_module("musclex.ui.QuadrantFoldingGUI")

                original_get_file = qf_module.getAFile
                qf_module.getAFile = lambda **_kwargs: str(settings_path)
            else:
                original_get_open_file_name = QFileDialog.getOpenFileName
                QFileDialog.getOpenFileName = lambda *_args, **_kwargs: (
                    str(settings_path),
                    "",
                )
            self.window.loadSettings()
            if self.module == "pt" and not self.window.boxes:
                raise RuntimeError(f"No PT boxes loaded from {settings_path}")
            QTimer.singleShot(0, self._start_processing)
        except Exception as exc:
            self._fail(f"Could not load settings {settings_path}: {exc}")
        finally:
            if original_get_file is not None:
                qf_module = importlib.import_module("musclex.ui.QuadrantFoldingGUI")

                qf_module.getAFile = original_get_file
            if original_get_open_file_name is not None:
                QFileDialog.getOpenFileName = original_get_open_file_name

    def _start_processing(self):
        if self.error or self.window is None:
            return
        try:
            if self.module == "qf" and not self.window.ableToProcess():
                QTimer.singleShot(100, self._start_processing)
                return
            if self.module == "pt" and (
                self.window.progressBar.isVisible()
                or self.window.threadPool.activeThreadCount() > 0
                or not self.window.tasksQueue.empty()
                or self.window.currentTask is not None
            ):
                QTimer.singleShot(100, self._start_processing)
                return
            self._restore_pt_template_update()
            if self.module == "qf" and not self.qf_background_applied:
                self.window.applyBGSub(skip_confirm=True)
                if not self.window.toggleFoldImage.isChecked():
                    raise RuntimeError(
                        "Applying QF background subtraction did not enable its checkbox"
                    )
                self.qf_background_applied = True
                if (
                    self.window._singleProcessing
                    or self.window._single_processing_inflight
                ):
                    QTimer.singleShot(100, self._start_processing)
                    return
            button = self.window.navControls.processFolderButton
            if not button.isEnabled():
                raise RuntimeError("The GUI's Process Current Folder button is disabled")
            self.processing_requested = True
            button.click()
        except Exception as exc:
            self._fail(f"Could not start GUI folder processing: {exc}")

    def _restore_pt_template_update(self):
        if self.original_pt_template_update is not None and self.pt_gui_class is not None:
            print("DEBUG: restoring PT template update")
            self.pt_gui_class._update_folder_cache_from_results = (
                self.original_pt_template_update
            )
            self.original_pt_template_update = None
            self.pt_gui_class = None

    def _poll(self):
        if time.monotonic() - self.start_time > self.timeout:
            self._fail(f"Timed out processing {self.dataset_dir}")
            return

        self._handle_modal_dialog()
        if self.error:
            return
        if self.window is None:
            return

        if self.module == "qf":
            task_stats = self.window.taskManager.get_statistics()
            processing = (
                getattr(self.window, "batchProcessing", False)
                or self.window.processExecutor is not None
                or self.window.pendingJobArgs
                or task_stats["pending"]
            )
            if processing:
                self.started = True
                return
            if not self.processing_requested or not self.started:
                return
        else:
            if self.window.progressBar.isVisible():
                if self.processing_requested:
                    self.started = True
                return
            if not self.processing_requested or not self.started:
                return
            if self.window.threadPool.activeThreadCount() > 0:
                return
            if not self.window.tasksQueue.empty():
                return

        self.timer.stop()
        self.app.quit()

    def _handle_modal_dialog(self):
        dialog = QApplication.activeModalWidget()
        if dialog is None:
            return

        if isinstance(dialog, QMessageBox):
            buttons = dialog.standardButtons()
            text = f"{dialog.windowTitle()} {dialog.text()} {dialog.informativeText()}"
            if (
                self.module == "qf"
                and "folder finished processing" in text.lower()
            ):
                self.started = True
                failed_tasks = getattr(self.window, "failedTaskErrors", {})
                save_errors = getattr(self.window, "saveErrors", {})
                if failed_tasks:
                    self.dialog_errors.append(
                        f"QF processing failed for: {', '.join(sorted(failed_tasks))}"
                    )
                if save_errors:
                    self.dialog_errors.append(
                        f"QF result saving failed for: {', '.join(sorted(save_errors))}"
                    )
                close_button = next(
                    (
                        button
                        for button in dialog.buttons()
                        if button.text().strip().lower() == "close"
                    ),
                    None,
                )
                if close_button is not None:
                    close_button.click()
                else:
                    self.dialog_errors.append(
                        "QF processing completion dialog did not have a Close button"
                    )
                    dialog.close()
                return

            if buttons & QMessageBox.Yes:
                if "process" in text.lower() and "image" in text.lower():
                    yes_button = dialog.button(QMessageBox.Yes)
                    if yes_button is not None:
                        yes_button.click()
                        return
                self.dialog_errors.append(f"Unexpected confirmation dialog: {text}")
                close_button = dialog.button(QMessageBox.Cancel)
                if close_button is None:
                    close_button = dialog.button(QMessageBox.No)
                if close_button is not None:
                    close_button.click()
                return

            if any(word in text.lower() for word in ("error", "failed", "failure")):
                self.dialog_errors.append(text.strip())
            ok_button = dialog.button(QMessageBox.Ok)
            if ok_button is not None:
                ok_button.click()
            return

        if isinstance(dialog, QDialog):
            for button in dialog.findChildren(QPushButton):
                if button.text().strip().lower() == "ok":
                    button.click()
                    return

    def _verify_summary(self, summary_path, images):
        if not summary_path.is_file():
            raise RuntimeError(f"GUI did not create {summary_path}")

        with summary_path.open(newline="", encoding="utf-8-sig") as csv_file:
            reader = csv.DictReader(csv_file)
            rows = list(reader)

        if not rows or not reader.fieldnames or "Filename" not in reader.fieldnames:
            raise RuntimeError(
                f"GUI summary is empty or missing Filename: {summary_path}"
            )

        generated_names = [row["Filename"] for row in rows]
        expected_names = {image.name for image in images}
        generated_name_set = set(generated_names)
        missing_names = expected_names - generated_name_set
        extra_names = generated_name_set - expected_names
        if missing_names or extra_names or len(generated_names) != len(expected_names):
            raise RuntimeError(
                "GUI summary does not contain exactly one row for every input image. "
                f"Missing: {sorted(missing_names)}; extra: {sorted(extra_names)}; "
                f"rows: {len(generated_names)}, inputs: {len(expected_names)}."
            )

    def _verify_qf_summary(self, summary_path):
        with summary_path.open(newline="", encoding="utf-8-sig") as csv_file:
            reader = csv.DictReader(csv_file)
            rows = list(reader)
            fieldnames = set(reader.fieldnames or ())
        required_fields = (
            "blank_enabled",
            "mask_enabled",
            "blank_weight",
            "n_peaks",
            "peak_width",
        )
        missing_columns = set(required_fields) - fieldnames
        if missing_columns:
            raise RuntimeError(
                "QF GUI summary is missing processed result columns: "
                + ", ".join(sorted(missing_columns))
            )

        for row in rows:
            filename = row["Filename"]
            expected = self.qf_expected_summary.get(filename)
            if expected is None:
                raise RuntimeError(
                    f"QF GUI summary has no captured processing result for {filename}"
                )
            for column in required_fields:
                actual_value = row[column]
                expected_value = expected.get(column)
                if actual_value == "" or expected_value is None:
                    raise RuntimeError(
                        f"QF GUI summary is missing processed {column} for {filename}"
                    )
                if isinstance(expected_value, bool):
                    matches = actual_value.strip().lower() == str(expected_value).lower()
                else:
                    try:
                        matches = float(actual_value) == float(expected_value)
                    except (TypeError, ValueError):
                        matches = actual_value == str(expected_value)
                if not matches:
                    raise RuntimeError(
                        f"QF GUI summary {column} for {filename} is {actual_value!r}; "
                        f"the processed value was {expected_value!r}"
                    )

    def _fail(self, message):
        self.error = message
        self.timer.stop()
        self.app.quit()


def _backup_name(path):
    stamp = datetime.now().strftime("%Y%m%d-%H%M%S")
    backup = path.with_name(f"{path.name}.backup-{stamp}")
    suffix = 1
    while backup.exists():
        backup = path.with_name(f"{path.name}.backup-{stamp}-{suffix}")
        suffix += 1
    return backup


def _promote_baseline(result_dir, target_dir, replace):
    backup_dir = None
    if target_dir.exists():
        if not replace:
            raise FileExistsError(
                f"GUI baseline already exists: {target_dir}. "
                "Pass --replace to preserve it as a timestamped backup and replace it."
            )
        backup_dir = _backup_name(target_dir)
        target_dir.rename(backup_dir)

    try:
        result_dir.rename(target_dir)
    except Exception:
        if backup_dir is not None and backup_dir.exists() and not target_dir.exists():
            backup_dir.rename(target_dir)
        raise

    if backup_dir is not None:
        print(f"Previous baseline preserved at {backup_dir}")


def _run_tests():
    command = [
        sys.executable,
        "-m",
        "unittest",
        "musclex.tests.musclex_tester.MuscleXGlobalTester",
        "-v",
    ]
    print("\nRunning the headless regression suite:", " ".join(command), flush=True)
    return subprocess.run(command, cwd=REPO_ROOT, check=False).returncode


def main(argv=None):
    parser = argparse.ArgumentParser(
        description=(
            "Regenerate QF/PT GUI CSV baselines using the GUI's settings and "
            "Process Current Folder workflows, then run the headless tests."
        )
    )
    parser.add_argument(
        "--module",
        choices=("qf", "pt", "all"),
        default="all",
        help="Which GUI module to run (default: all)",
    )
    parser.add_argument(
        "--dataset",
        action="append",
        help="Dataset directory name; may be repeated. Defaults to all selected-module datasets.",
    )
    parser.add_argument(
        "--replace",
        action="store_true",
        help="Replace existing GUI baselines, preserving the old folders as backups.",
    )
    parser.add_argument(
        "--skip-tests",
        action="store_true",
        help="Generate baselines without running MuscleXGlobalTester afterward.",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=900,
        help="Maximum seconds allowed for each dataset (default: 900)",
    )
    args = parser.parse_args(argv)

    modules = ("qf", "pt") if args.module == "all" else (args.module,)
    allowed_datasets = {
        dataset for module in modules for dataset in DATASETS[module]
    }
    selected_datasets = args.dataset or [
        dataset for module in modules for dataset in DATASETS[module]
    ]
    unknown_datasets = set(selected_datasets) - allowed_datasets
    if unknown_datasets:
        parser.error(
            "Dataset(s) are not valid for the selected module(s): "
            + ", ".join(sorted(unknown_datasets))
        )
    if args.timeout <= 0:
        parser.error("--timeout must be greater than zero")

    targets = [
        (module, IMAGE_ROOT / dataset)
        for module in modules
        for dataset in selected_datasets
        if dataset in DATASETS[module]
    ]
    for module, dataset_dir in targets:
        if not dataset_dir.is_dir():
            parser.error(f"Dataset directory does not exist: {dataset_dir}")
        target_dir = dataset_dir / f"{module}_results_gui"
        if target_dir.exists() and not args.replace:
            parser.error(
                f"GUI baseline already exists: {target_dir}. "
                "Pass --replace to preserve and replace it."
            )

    from PySide6.QtWidgets import QApplication

    from .. import stylesheet

    app = QApplication.instance() or QApplication(sys.argv[:1])
    app.setQuitOnLastWindowClosed(False)
    app.setStyle("Fusion")
    app.setStyleSheet(stylesheet.stylesheet)

    failures = []
    for module, dataset_dir in targets:
        print(f"\nGenerating {module.upper()} GUI baseline for {dataset_dir}")
        staging_dir = Path(
            tempfile.mkdtemp(prefix=f".{module}-gui-baseline-", dir=dataset_dir)
        )
        try:
            run = BaselineRun(
                app, module, dataset_dir, staging_dir, timeout=args.timeout
            )
            result_dir = run.run()
            target_dir = dataset_dir / f"{module}_results_gui"
            _promote_baseline(result_dir, target_dir, args.replace)
            print(f"GUI baseline saved: {target_dir / 'summary.csv'}")
        except Exception as exc:
            failures.append((dataset_dir.name, str(exc)))
            print(
                f"FAILED: {dataset_dir.name}: {exc}\n"
                f"Staged output retained at: {staging_dir}",
                file=sys.stderr,
            )
        else:
            shutil.rmtree(staging_dir)

    if failures:
        print("\nGUI baseline generation failed:", file=sys.stderr)
        for dataset, message in failures:
            print(f"  {dataset}: {message}", file=sys.stderr)
        return 1

    if not args.skip_tests:
        return _run_tests()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
