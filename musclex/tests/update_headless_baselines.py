"""Copy generated headless CSV results into the committed test references."""

import argparse
import csv
import shutil
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
IMAGE_ROOT = REPO_ROOT / "musclex" / "tests" / "testImages"
RESULT_ROOT = REPO_ROOT / "musclex" / "tests" / "testResults"
DATASETS = {
    "qf": ("EIGERimages", "MARimages", "PILATUSimages"),
    "eq": ("EIGERimages", "MARimages", "PILATUSimages"),
    "di": ("EIGERimages", "MARimages", "PILATUSimages"),
    "pt": (
        "EIGER_PT_Convex_Hull_Vertical",
        "MAR_PT_Convex_Hull_Vertical",
        "PT_FittingGaussians_Horizontal",
    ),
}
SUMMARY_NAMES = {
    "qf": "summary.csv",
    "eq": "summary2.csv",
    "di": "summary.csv",
    "pt": "summary.csv",
}


def _summary_paths(module, dataset):
    summary_name = SUMMARY_NAMES[module]
    source = IMAGE_ROOT / dataset / f"{module}_results" / summary_name
    target_dir = RESULT_ROOT / dataset
    if module != "pt":
        target_dir /= f"{module}_results"
    return source, target_dir / summary_name


def _validate_summary(path):
    try:
        with path.open(newline="", encoding="utf-8-sig") as csv_file:
            reader = csv.DictReader(csv_file)
            if not reader.fieldnames or not any(
                field.lower() == "filename" for field in reader.fieldnames
            ):
                raise ValueError("CSV must have a Filename column")
            if next(reader, None) is None:
                raise ValueError("CSV must contain at least one result row")
    except (OSError, UnicodeError, csv.Error, ValueError) as exc:
        raise ValueError(f"Invalid headless summary {path}: {exc}") from exc


def main(argv=None):
    parser = argparse.ArgumentParser(
        description=(
            "Copy generated headless CSV summaries to committed test references. "
            "Without --apply, only validate and preview the updates."
        )
    )
    parser.add_argument(
        "--module",
        choices=(*DATASETS, "all"),
        default="all",
        help="Which module's baselines to update (default: all)",
    )
    parser.add_argument(
        "--dataset",
        action="append",
        help="Dataset directory name; may be repeated to update selected datasets.",
    )
    parser.add_argument(
        "--apply",
        action="store_true",
        help="Write the generated summaries to testResults (default is preview only).",
    )
    args = parser.parse_args(argv)

    modules = tuple(DATASETS) if args.module == "all" else (args.module,)
    allowed_datasets = {
        dataset for module in modules for dataset in DATASETS[module]
    }
    selected_datasets = list(
        dict.fromkeys(
            args.dataset
            or [dataset for module in modules for dataset in DATASETS[module]]
        )
    )
    unknown_datasets = set(selected_datasets) - allowed_datasets
    if unknown_datasets:
        parser.error(
            "Dataset(s) are not valid for the selected module(s): "
            + ", ".join(sorted(unknown_datasets))
        )

    updates = [
        (module, dataset, *_summary_paths(module, dataset))
        for module in modules
        for dataset in selected_datasets
        if dataset in DATASETS[module]
    ]
    errors = []
    for module, dataset, source, _target in updates:
        try:
            _validate_summary(source)
        except ValueError as exc:
            errors.append(str(exc))

    if errors:
        for error in errors:
            print(error, file=sys.stderr)
        print(
            "\nNo baselines were changed. Run the relevant headless tests first.",
            file=sys.stderr,
        )
        return 1

    action = "Updating" if args.apply else "Would update"
    for _module, _dataset, source, target in updates:
        print(f"{action}: {source} -> {target}")

    if args.apply:
        for _module, _dataset, source, target in updates:
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)
        print(f"\nUpdated {len(updates)} headless baseline file(s).")
    else:
        print(
            f"\nValidated {len(updates)} headless summary file(s). "
            "Pass --apply to copy them into testResults."
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
