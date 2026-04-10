"""
AssociationStore: Persists input→output directory associations per user.

Stored in ``~/.musclex/directory_associations.json``, keyed on the
canonical absolute path of the input directory.

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

import json
import os
from pathlib import Path
from typing import Optional


_DEFAULT_PATH = Path.home() / ".musclex" / "directory_associations.json"


class AssociationStore:
    """Read / write ``~/.musclex/directory_associations.json``."""

    def __init__(self, path: Optional[Path] = None):
        self._path = path or _DEFAULT_PATH
        self._data: dict = {}
        self._load()

    # ---- public API ----

    def lookup(self, input_dir: str) -> Optional[str]:
        """Return the stored output directory for *input_dir*, or ``None``.

        Returns ``None`` when no association exists **or** the stored
        output directory no longer exists on disk.
        """
        key = os.path.realpath(input_dir)
        output = self._data.get(key)
        if output and os.path.isdir(output):
            return output
        return None

    def save(self, input_dir: str, output_dir: str) -> None:
        """Persist an input→output mapping (overwrites any previous one)."""
        key = os.path.realpath(input_dir)
        self._data[key] = os.path.realpath(output_dir)
        self._flush()

    def remove(self, input_dir: str) -> None:
        """Delete the association for *input_dir* (no-op if absent)."""
        key = os.path.realpath(input_dir)
        if key in self._data:
            del self._data[key]
            self._flush()

    # ---- internals ----

    def _load(self) -> None:
        try:
            if self._path.exists():
                with open(self._path, "r", encoding="utf-8") as f:
                    self._data = json.load(f)
        except Exception:
            self._data = {}

    def _flush(self) -> None:
        try:
            self._path.parent.mkdir(parents=True, exist_ok=True)
            with open(self._path, "w", encoding="utf-8") as f:
                json.dump(self._data, f, indent=2)
        except Exception as exc:
            print(f"Warning: could not save directory associations: {exc}")
