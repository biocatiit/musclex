"""
SettingsManager: Centralized manager for the settings/ directory.

Phase 1 covers geometry (manual center / rotation) and auto-geometry cache.
Future phases will add calibration and blank/mask management.

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
from pathlib import Path
from typing import Optional, Tuple


class SettingsManager:
    """Centralized manager for the ``settings/`` directory.

    Holds in-memory caches and owns all JSON I/O for:

    * **Manual geometry** – ``center_settings.json``, ``rotation_settings.json``
    * **Auto-geometry cache** – ``auto_geometry_cache.json``

    The class is pure Python (no Qt dependency) so it can be used in
    headless / batch processing contexts.
    """

    def __init__(self, settings_dir: str = ""):
        self._settings_dir = settings_dir

        # Manual geometry  {"filename": {"center": [x,y], "source": "..."}}
        self._center: dict = {}
        # Manual rotation  {"filename": {"rotation": angle, "source": "..."}}
        self._rotation: dict = {}
        # Auto cache       {"filename": {"center": [x,y], "rotation": angle}}
        self._auto_cache: dict = {}
        # Global base      {"center": [x,y], "rotation": angle, "base_image": filename}
        self._global_base: dict = {}

        if settings_dir:
            self.load()

    # ===== Properties =====

    @property
    def settings_dir(self) -> str:
        return self._settings_dir

    @property
    def center_settings(self) -> dict:
        """Direct dict access (read-only intent) for backward compatibility."""
        return self._center

    @property
    def rotation_settings(self) -> dict:
        """Direct dict access (read-only intent) for backward compatibility."""
        return self._rotation

    @property
    def auto_cache(self) -> dict:
        return self._auto_cache

    # ===== Manual center =====

    def get_center(self, filename: str) -> Optional[Tuple[float, float]]:
        data = self._center.get(filename)
        if data and 'center' in data:
            return tuple(data['center'])
        return None

    def get_center_data(self, filename: str) -> Optional[dict]:
        """Return the full center entry (center + source), or None."""
        return self._center.get(filename)

    def set_center(self, filename: str, center, source: str):
        self._center[filename] = {
            'center': list(center),
            'source': source,
        }

    def clear_center(self, filename: str):
        self._center.pop(filename, None)

    def has_manual_center(self, filename: str) -> bool:
        return filename in self._center

    # ===== Manual rotation =====

    def get_rotation(self, filename: str) -> Optional[float]:
        data = self._rotation.get(filename)
        if data and 'rotation' in data:
            return data['rotation']
        return None

    def get_rotation_data(self, filename: str) -> Optional[dict]:
        return self._rotation.get(filename)

    def set_rotation(self, filename: str, rotation: float, source: str):
        self._rotation[filename] = {
            'rotation': rotation,
            'source': source,
        }

    def clear_rotation(self, filename: str):
        self._rotation.pop(filename, None)

    def has_manual_rotation(self, filename: str) -> bool:
        return filename in self._rotation

    # ===== Auto-geometry cache =====

    def get_auto_center(self, filename: str) -> Optional[Tuple[float, float]]:
        data = self._auto_cache.get(filename)
        if data and data.get('center'):
            return tuple(data['center'])
        return None

    def get_auto_rotation(self, filename: str) -> Optional[float]:
        data = self._auto_cache.get(filename)
        if data and data.get('rotation') is not None:
            return data['rotation']
        return None

    def set_auto_cache(self, filename: str, center, rotation):
        self._auto_cache[filename] = {
            'center': list(center) if center else None,
            'rotation': rotation if rotation is not None else None,
        }

    # ===== Global base =====

    def get_global_base(self) -> dict:
        """Return the global base dict (may be empty)."""
        return self._global_base

    def set_global_base(self, center, rotation, base_filename: str,
                        center_source: str = '', rotation_source: str = ''):
        self._global_base = {
            'center': list(center) if center else None,
            'center_source': center_source,
            'rotation': rotation,
            'rotation_source': rotation_source,
            'base_image': base_filename,
        }

    def clear_global_base(self):
        self._global_base = {}

    # ===== I/O =====

    def _settings_path(self) -> Path:
        return Path(self._settings_dir) / "settings"

    def load(self):
        """Load all JSON files from disk into memory."""
        self._load_center()
        self._load_rotation()
        self._load_auto_cache()
        self._load_global_base()

    def _load_center(self):
        path = self._settings_path() / "center_settings.json"
        if path.exists():
            try:
                with open(path, 'r') as f:
                    self._center = json.load(f)
                print(f"Loaded center settings for {len(self._center)} images")
            except Exception as e:
                print(f"Error loading center settings: {e}")
                self._center = {}
        else:
            self._center = {}

    def _load_rotation(self):
        path = self._settings_path() / "rotation_settings.json"
        if path.exists():
            try:
                with open(path, 'r') as f:
                    self._rotation = json.load(f)
                print(f"Loaded rotation settings for {len(self._rotation)} images")
            except Exception as e:
                print(f"Error loading rotation settings: {e}")
                self._rotation = {}
        else:
            self._rotation = {}

    def _load_auto_cache(self):
        path = self._settings_path() / "auto_geometry_cache.json"
        if path.exists():
            try:
                with open(path, 'r') as f:
                    self._auto_cache = json.load(f)
            except Exception as e:
                print(f"Error loading auto geometry cache: {e}")
                self._auto_cache = {}
        else:
            self._auto_cache = {}

    def _load_global_base(self):
        path = self._settings_path() / "global_base.json"
        if path.exists():
            try:
                with open(path, 'r') as f:
                    self._global_base = json.load(f)
            except Exception as e:
                print(f"Error loading global base settings: {e}")
                self._global_base = {}
        else:
            self._global_base = {}

    def save_center(self):
        if not self._settings_dir:
            return
        d = self._settings_path()
        d.mkdir(exist_ok=True)
        try:
            with open(d / "center_settings.json", 'w') as f:
                json.dump(self._center, f, indent=2)
            print(f"Saved center settings for {len(self._center)} images")
        except Exception as e:
            print(f"Error saving center settings: {e}")

    def save_rotation(self):
        if not self._settings_dir:
            return
        d = self._settings_path()
        d.mkdir(exist_ok=True)
        try:
            with open(d / "rotation_settings.json", 'w') as f:
                json.dump(self._rotation, f, indent=2)
            print(f"Saved rotation settings for {len(self._rotation)} images")
        except Exception as e:
            print(f"Error saving rotation settings: {e}")

    def save_auto_cache(self):
        if not self._settings_dir:
            return
        d = self._settings_path()
        d.mkdir(parents=True, exist_ok=True)
        try:
            with open(d / "auto_geometry_cache.json", 'w') as f:
                json.dump(self._auto_cache, f, indent=2)
        except Exception as e:
            print(f"Error saving auto geometry cache: {e}")

    def save_global_base(self):
        if not self._settings_dir:
            return
        d = self._settings_path()
        d.mkdir(exist_ok=True)
        try:
            with open(d / "global_base.json", 'w') as f:
                json.dump(self._global_base, f, indent=2)
        except Exception as e:
            print(f"Error saving global base settings: {e}")

    def save_all(self):
        self.save_center()
        self.save_rotation()
        self.save_auto_cache()
        self.save_global_base()

    # ===== Directory switching =====

    def switch_dir(self, new_settings_dir: str):
        """Save current state, switch directory, reload."""
        if new_settings_dir == self._settings_dir:
            return
        if self._settings_dir:
            self.save_all()
        self._settings_dir = new_settings_dir
        if new_settings_dir:
            self.load()
        else:
            self._center = {}
            self._rotation = {}
            self._auto_cache = {}
            self._global_base = {}
        print(f"Settings directory updated to: {new_settings_dir}")
