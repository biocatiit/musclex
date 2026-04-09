"""
SettingsManager: Centralized manager for the settings/ directory.

Owns all file I/O under ``<dataset>/settings/``:

* **Geometry** – center, rotation, auto-geometry cache, global base
* **Flags** – transform, ignore, image-diff cache
* **Blank / mask state** – enable/disable flags, blank config JSON
* **Calibration** – ``calibration.info`` (pickle), ``calibrationDialog.json``

The class is pure Python (no Qt dependency) so it can be used in
headless / batch processing contexts.

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
import pickle
from pathlib import Path
from typing import Optional, Tuple


class SettingsManager:
    """Centralized manager for the ``settings/`` directory.

    Single owner of all file I/O under ``<dataset>/settings/``.
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
        # Transform flags  {"filename": true}  — images marked to be transformed in calculation
        self._transform: dict = {}
        # Ignore flags  {"filename": true}  — images excluded from alignment calculations
        self._ignore: dict = {}
        # Image diff cache  {"filename": float}  — mean abs diff vs previous image
        self._image_diff: dict = {}

        if settings_dir:
            self.load()

    # ===== Properties =====

    @property
    def settings_dir(self) -> str:
        return self._settings_dir

    @property
    def settings_path(self) -> Path:
        """Absolute path to the ``settings/`` sub-directory."""
        return Path(self._settings_dir) / "settings"

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

    # ===== Transform flags =====

    def has_transform(self, filename: str) -> bool:
        return bool(self._transform.get(filename))

    def set_transform(self, filename: str):
        self._transform[filename] = True

    def clear_transform(self, filename: str):
        self._transform.pop(filename, None)

    # ===== Ignore flags =====

    def has_ignore(self, filename: str) -> bool:
        return bool(self._ignore.get(filename))

    def set_ignore(self, filename: str):
        self._ignore[filename] = True

    def clear_ignore(self, filename: str):
        self._ignore.pop(filename, None)

    # ===== Image diff cache =====

    def get_image_diff(self, filename: str) -> Optional[float]:
        return self._image_diff.get(filename)

    def has_image_diff(self, filename: str) -> bool:
        return filename in self._image_diff

    def set_image_diff(self, filename: str, value: float):
        self._image_diff[filename] = value

    def clear_image_diff(self, filename: str):
        self._image_diff.pop(filename, None)

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

    def set_global_base(self, base_filename: str):
        self._global_base = {
            'base_image': base_filename,
        }

    def clear_global_base(self):
        self._global_base = {}

    # ===== Blank / mask state =====

    @property
    def blank_config_path(self) -> Path:
        return self.settings_path / "blank_image_settings.json"

    @property
    def mask_tif_path(self) -> Path:
        return self.settings_path / "mask.tif"

    def has_blank_config(self) -> bool:
        """True when ``blank_image_settings.json`` exists on disk."""
        return self.blank_config_path.exists()

    def has_mask_file(self) -> bool:
        """True when ``mask.tif`` exists on disk."""
        return self.mask_tif_path.exists()

    @property
    def blank_enabled(self) -> bool:
        """Config exists AND the disabled-flag is absent."""
        return (self.has_blank_config()
                and not (self.settings_path / ".blank_image_disabled").exists())

    @property
    def mask_enabled(self) -> bool:
        """mask.tif exists AND the disabled-flag is absent."""
        return (self.has_mask_file()
                and not (self.settings_path / ".mask_disabled").exists())

    @property
    def blank_weight(self) -> float:
        """Read the weight from ``blank_image_settings.json`` (default 1.0)."""
        if not self.has_blank_config():
            return 1.0
        try:
            with open(self.blank_config_path, 'r') as f:
                return json.load(f).get('weight', 1.0)
        except Exception:
            return 1.0

    def set_blank_enabled(self, enabled: bool):
        """Create or remove the ``.blank_image_disabled`` flag file."""
        flag = self.settings_path / ".blank_image_disabled"
        if enabled:
            if flag.exists():
                flag.unlink()
        else:
            self.settings_path.mkdir(exist_ok=True)
            flag.touch()

    def set_mask_enabled(self, enabled: bool):
        """Create or remove the ``.mask_disabled`` flag file."""
        flag = self.settings_path / ".mask_disabled"
        if enabled:
            if flag.exists():
                flag.unlink()
        else:
            self.settings_path.mkdir(exist_ok=True)
            flag.touch()

    # ----- image loading -----

    def load_blank_and_mask(self):
        """Load blank image and mask from ``settings/``.

        Returns:
            tuple: ``(blank_img, mask, weight)`` where each image is a
            numpy array or *None*, and *weight* is a float (default 1.0).
        """
        import fabio

        mask = None
        blank_img = None
        blank_weight = 1.0

        if self.mask_tif_path.exists():
            mask = fabio.open(str(self.mask_tif_path)).data

        blank_config_file = self.blank_config_path
        if blank_config_file.exists():
            try:
                with open(blank_config_file, 'r') as f:
                    config = json.load(f)
                file_path = Path(config.get('file_path', ''))
                blank_weight = config.get('weight', 1.0)
                if file_path.exists():
                    blank_img = fabio.open(str(file_path)).data
            except Exception as e:
                print(f"Failed to load blank image from JSON config: {e}")

        # Legacy fallback: blank.tif
        if blank_img is None:
            legacy = self.settings_path / "blank.tif"
            if legacy.exists():
                blank_img = fabio.open(str(legacy)).data

        return blank_img, mask, blank_weight

    def load_mask_only(self):
        """Load only ``mask.tif``.

        Returns:
            numpy array or *None*.
        """
        import fabio

        if self.mask_tif_path.exists():
            return fabio.open(str(self.mask_tif_path)).data
        return None

    # ===== Calibration =====

    def load_calibration_cache(self) -> Optional[dict]:
        """Load ``calibration.info`` (pickle). Returns the cache dict or None."""
        path = self.settings_path / "calibration.info"
        if path.exists():
            try:
                with open(path, 'rb') as f:
                    cache = pickle.load(f)
                if cache is not None and "version" in cache:
                    return cache
            except Exception as e:
                print(f"Warning: Failed to load calibration cache: {e}")
        return None

    def save_calibration_cache(self, cache: dict):
        """Write ``calibration.info`` (pickle)."""
        if not self._settings_dir:
            return
        d = self.settings_path
        d.mkdir(exist_ok=True)
        try:
            with open(d / "calibration.info", 'wb') as f:
                pickle.dump(cache, f)
        except Exception as e:
            print(f"Error saving calibration cache: {e}")

    def load_calibration_dialog(self) -> Optional[dict]:
        """Load ``calibrationDialog.json``. Returns the dict or None."""
        path = self.settings_path / "calibrationDialog.json"
        if path.exists():
            try:
                with open(path, 'r') as f:
                    return json.load(f)
            except Exception as e:
                print(f"Error loading calibrationDialog.json: {e}")
        return None

    def save_calibration_dialog(self, info: dict):
        """Write ``calibrationDialog.json``."""
        if not self._settings_dir:
            return
        d = self.settings_path
        d.mkdir(exist_ok=True)
        try:
            with open(d / "calibrationDialog.json", 'w') as f:
                json.dump(info, f)
        except Exception as e:
            print(f"Error saving calibrationDialog.json: {e}")

    # ===== I/O =====

    def load(self):
        """Load all JSON files from disk into memory."""
        self._load_center()
        self._load_rotation()
        self._load_auto_cache()
        self._load_global_base()
        self._load_transform()
        self._load_ignore()
        self._load_image_diff()

    def _load_center(self):
        path = self.settings_path / "center_settings.json"
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
        path = self.settings_path / "rotation_settings.json"
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
        path = self.settings_path / "auto_geometry_cache.json"
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
        path = self.settings_path / "global_base.json"
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
        d = self.settings_path
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
        d = self.settings_path
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
        d = self.settings_path
        d.mkdir(parents=True, exist_ok=True)
        try:
            with open(d / "auto_geometry_cache.json", 'w') as f:
                json.dump(self._auto_cache, f, indent=2)
        except Exception as e:
            print(f"Error saving auto geometry cache: {e}")

    def save_global_base(self):
        if not self._settings_dir:
            return
        d = self.settings_path
        d.mkdir(exist_ok=True)
        try:
            with open(d / "global_base.json", 'w') as f:
                json.dump(self._global_base, f, indent=2)
        except Exception as e:
            print(f"Error saving global base settings: {e}")

    def _load_transform(self):
        path = self.settings_path / "transform_settings.json"
        if path.exists():
            try:
                with open(path, 'r') as f:
                    self._transform = json.load(f)
            except Exception as e:
                print(f"Error loading transform settings: {e}")
                self._transform = {}
        else:
            self._transform = {}

    def save_transform(self):
        if not self._settings_dir:
            return
        d = self.settings_path
        d.mkdir(exist_ok=True)
        try:
            with open(d / "transform_settings.json", 'w') as f:
                json.dump(self._transform, f, indent=2)
        except Exception as e:
            print(f"Error saving transform settings: {e}")

    def _load_ignore(self):
        path = self.settings_path / "ignore_settings.json"
        if path.exists():
            try:
                with open(path, 'r') as f:
                    self._ignore = json.load(f)
                print(f"Loaded ignore settings for {len(self._ignore)} images")
            except Exception as e:
                print(f"Error loading ignore settings: {e}")
                self._ignore = {}
        else:
            self._ignore = {}

    def save_ignore(self):
        if not self._settings_dir:
            return
        d = self.settings_path
        d.mkdir(exist_ok=True)
        try:
            with open(d / "ignore_settings.json", 'w') as f:
                json.dump(self._ignore, f, indent=2)
        except Exception as e:
            print(f"Error saving ignore settings: {e}")

    def _load_image_diff(self):
        path = self.settings_path / "image_diff_cache.json"
        if path.exists():
            try:
                with open(path, 'r') as f:
                    self._image_diff = json.load(f)
            except Exception as e:
                print(f"Error loading image diff cache: {e}")
                self._image_diff = {}
        else:
            self._image_diff = {}

    def save_image_diff(self):
        if not self._settings_dir:
            return
        d = self.settings_path
        d.mkdir(exist_ok=True)
        try:
            with open(d / "image_diff_cache.json", 'w') as f:
                json.dump(self._image_diff, f, indent=2)
        except Exception as e:
            print(f"Error saving image diff cache: {e}")

    def save_all(self):
        self.save_center()
        self.save_rotation()
        self.save_auto_cache()
        self.save_global_base()
        self.save_transform()
        self.save_ignore()
        self.save_image_diff()

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
            self._transform = {}
            self._ignore = {}
            self._image_diff = {}
        print(f"Settings directory updated to: {new_settings_dir}")
