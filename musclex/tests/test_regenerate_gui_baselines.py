import csv
import tempfile
import unittest
from pathlib import Path

from .regenerate_gui_baselines import (
    DATASETS,
    IMAGE_ROOT,
    BaselineRun,
    _build_targets,
    _promote_baseline,
)


class VerifySummaryTests(unittest.TestCase):
    def verify_summary(self, module, rows, images=("image1.tif", "image2.tif")):
        with tempfile.TemporaryDirectory() as temp_dir:
            summary_path = Path(temp_dir) / "summary.csv"
            filename_column = "filename" if module == "di" else "Filename"
            with summary_path.open("w", newline="", encoding="utf-8") as csv_file:
                writer = csv.DictWriter(csv_file, fieldnames=(filename_column,))
                writer.writeheader()
                writer.writerows({filename_column: name} for name in rows)

            run = BaselineRun.__new__(BaselineRun)
            run.module = module
            run._verify_summary(
                summary_path, [Path(image_name) for image_name in images]
            )

    def test_eq_summary_allows_multiple_rows_per_image(self):
        self.verify_summary(
            "eq",
            ("image1.tif", "image1.tif", "image2.tif", "image2.tif"),
        )

    def test_eq_summary_rejects_missing_or_extra_images(self):
        with self.assertRaisesRegex(RuntimeError, "exactly every input image"):
            self.verify_summary("eq", ("image1.tif", "other.tif"))

    def test_qf_and_di_summaries_reject_duplicate_rows(self):
        for module in ("qf", "di"):
            with self.subTest(module=module):
                with self.assertRaisesRegex(RuntimeError, "exactly one row"):
                    self.verify_summary(
                        module,
                        ("image1.tif", "image1.tif", "image2.tif"),
                    )


class BuildTargetsTests(unittest.TestCase):
    def test_overlapping_datasets_are_targeted_once_per_module(self):
        modules = ("qf", "eq", "di", "pt")
        selected_datasets = [
            dataset for module in modules for dataset in DATASETS[module]
        ]

        targets = _build_targets(modules, selected_datasets)
        expected = [
            (module, IMAGE_ROOT / dataset)
            for module in modules
            for dataset in DATASETS[module]
        ]

        self.assertEqual(targets, expected)


class PromoteBaselineTests(unittest.TestCase):
    def test_replace_removes_existing_baseline_without_creating_backup(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            result_dir = root / ".staged-results"
            target_dir = root / "qf_results_gui"
            result_dir.mkdir()
            target_dir.mkdir()
            (result_dir / "summary.csv").write_text("new baseline\n")
            (target_dir / "summary.csv").write_text("old baseline\n")

            _promote_baseline(result_dir, target_dir, replace=True)

            self.assertFalse(result_dir.exists())
            self.assertEqual(
                (target_dir / "summary.csv").read_text(), "new baseline\n"
            )
            self.assertEqual(list(root.glob("*.backup-*")), [])


if __name__ == "__main__":
    unittest.main()
