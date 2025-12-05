"""
ImageData: Unified container for diffraction images.

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

import os
import copy
import numpy as np
from typing import Optional, Tuple, Dict, Any
from pathlib import Path

from .file_manager import getBlankImageAndMask, getMaskOnly
from .image_processor import getCenter, processImageForIntCenter, getRotationAngle


class ImageData:
    """
    Container for diffraction image data, geometry, and preprocessing.
    
    Encapsulates:
    - Raw and processed image data
    - Geometric properties (center, rotation) with manual/auto calculation
    - Preprocessing configuration (blank image, mask)
    - Fingerprint generation for cache validation
    
    Usage:
        # Create from raw image
        img_data = ImageData(img, path, name)
        
        # Set manual geometry if needed
        img_data.set_manual_center((512, 512))
        
        # Pass to processor
        qf = QuadrantFolder(img_data, parent=self)
        pp = ProjectionProcessor(img_data)
    """
    
    def __init__(
        self,
        img: np.ndarray,
        img_path: str,
        img_name: str,
        # Manual geometry overrides
        center: Optional[Tuple[float, float]] = None,
        rotation: Optional[float] = None,
        # Blank/Mask config (provided by caller, not auto-detected)
        apply_blank: bool = False,
        apply_mask: bool = False,
        blank_weight: float = 1.0,
        # Other configs
        orientation_model: int = 0,
        detector: Optional[str] = None,
        invalid_pixel_threshold: float = -1.0,
        **kwargs
    ):
        """
        Initialize processing image container.
        
        Note: Blank/mask configuration should be provided by the caller (typically
        from ImageSettingsPanel). ImageData no longer auto-detects these settings.
        
        :param img: Raw image array
        :param img_path: Directory path containing the image
        :param img_name: Image filename (for caching and display)
        :param center: Manual center (x, y) or None for auto-calculation
        :param rotation: Manual rotation angle or None for auto-calculation
        :param apply_blank: Whether to apply blank image subtraction
        :param apply_mask: Whether to apply mask
        :param blank_weight: Weight for blank image subtraction
        :param orientation_model: Model index for rotation calculation
        :param detector: Detector type for pyFAI integration
        :param invalid_pixel_threshold: Value to set for masked pixels
        :param kwargs: Additional metadata
        """
        # Raw data (immutable)
        self._raw_img = np.asarray(img).astype("float32")
        self.img_path = Path(img_path)
        self.img_name = img_name
        
        # Manual geometry (takes precedence over computed)
        self._manual_center = tuple(center) if center is not None else None
        self._manual_rotation = rotation
        
        # Computed geometry (cached after calculation)
        self._computed_center: Optional[Tuple[float, float]] = None
        self._computed_rotation: Optional[float] = None
        
        # Preprocessing config (provided by caller, not auto-detected)
        self.apply_blank = apply_blank
        self.apply_mask = apply_mask
        self.blank_weight = blank_weight
        self.invalid_pixel_threshold = invalid_pixel_threshold
        self.orientation_model = orientation_model
        self.detector = detector
        
        # Lazy-loaded preprocessing data
        self._processed_img: Optional[np.ndarray] = None
        self._blank_img: Optional[np.ndarray] = None
        self._mask: Optional[np.ndarray] = None
        self._mask_only: Optional[np.ndarray] = None
        
        # State tracking
        self._preprocessing_applied = False
        self._blank_mask_loaded = False
        
        # Additional metadata
        self.metadata: Dict[str, Any] = kwargs
        
        # Load auto-calculated geometry from cache (if available)
        self._load_auto_cache()
    
    # ==================== Factory Methods ====================
    
    @classmethod
    def from_settings_panel(cls, img, img_path, img_name, settings_panel):
        """
        Factory method: Create ImageData from ImageSettingsPanel.
        
        This centralizes the logic of extracting settings from the panel
        and creating an ImageData object. GUI code becomes simpler and
        consistent across QuadrantFoldingGUI and ProjectionTracesGUI.
        
        Args:
            img: Image array (numpy.ndarray)
            img_path: Directory path (str or Path)
            img_name: Image filename (str)
            settings_panel: ImageSettingsPanel instance
        
        Returns:
            ImageData object configured with panel settings
        
        Usage:
            # In GUI code
            image_data = ImageData.from_settings_panel(
                img, self.file_manager.dir_path, img_name, self.image_settings_panel
            )
        """
        # Get manual center/rotation from panel
        manual_center, manual_rotation = settings_panel.get_manual_settings(img_name)
        
        # Get blank/mask configuration
        blank_mask_config = settings_panel.get_blank_mask_config()
        
        # Get orientation model (if available, default to 0)
        orientation_model = getattr(settings_panel, '_orientation_model', 0)
        
        # Create and return ImageData
        return cls(
            img=img,
            img_path=img_path,
            img_name=img_name,
            center=manual_center,
            rotation=manual_rotation,
            orientation_model=orientation_model,
            apply_blank=blank_mask_config['apply_blank'],
            apply_mask=blank_mask_config['apply_mask'],
            blank_weight=blank_mask_config['blank_weight']
        )
    
    # ==================== Properties ====================
    
    @property
    def shape(self) -> Tuple[int, int]:
        """Get image shape"""
        return self._raw_img.shape
    
    @property
    def center(self) -> Tuple[float, float]:
        """
        Get center: manual if set, otherwise auto-calculate (lazy).
        Always returns a valid center.
        """
        if self._manual_center is not None:
            return self._manual_center
        
        # Auto-calculate if not cached
        if self._computed_center is None:
            self.ensure_center()
        
        return self._computed_center
    
    @property
    def rotation(self) -> float:
        """
        Get rotation: manual if set, otherwise auto-calculate (lazy).
        Always returns a valid rotation.
        """
        if self._manual_rotation is not None:
            return self._manual_rotation
        
        # Auto-calculate if not cached
        if self._computed_rotation is None:
            self.ensure_rotation()
        
        return self._computed_rotation
    
    @property
    def has_manual_center(self) -> bool:
        """Check if center was manually set"""
        return self._manual_center is not None
    
    @property
    def has_manual_rotation(self) -> bool:
        """Check if rotation was manually set"""
        return self._manual_rotation is not None
    
    # ==================== Manual Geometry Setting ====================
    
    def set_manual_center(self, center: Tuple[float, float]):
        """
        Set center manually (overrides auto-calculation).
        
        :param center: (x, y) coordinates
        """
        self._manual_center = tuple(center)
        print(f"Manual center set: {center}")
    
    def set_manual_rotation(self, rotation: float):
        """
        Set rotation manually (overrides auto-calculation).
        
        :param rotation: angle in degrees
        """
        self._manual_rotation = rotation
        print(f"Manual rotation set: {rotation}°")
    
    def clear_manual_center(self):
        """Clear manual center (will use auto-calculation)"""
        self._manual_center = None
    
    def clear_manual_rotation(self):
        """Clear manual rotation (will use auto-calculation)"""
        self._manual_rotation = None
    
    def update_manual_center(self, center: Optional[Tuple[float, float]]):
        """
        Update manual center at runtime (for GUI interactions).
        
        Note: This updates the runtime value only. GUI is responsible for
        persisting to settings file if needed.
        
        :param center: New center (x, y) or None to use auto-calculation
        """
        self._manual_center = tuple(center) if center is not None else None
        print(f"Manual center updated: {self._manual_center}")
    
    def update_manual_rotation(self, rotation: Optional[float]):
        """
        Update manual rotation at runtime (for GUI interactions).
        
        Note: This updates the runtime value only. GUI is responsible for
        persisting to settings file if needed.
        
        :param rotation: New rotation angle in degrees or None to use auto-calculation
        """
        self._manual_rotation = rotation
        print(f"Manual rotation updated: {self._manual_rotation}")
    
    # ==================== Cache Management ====================
    
    def _load_auto_cache(self):
        """
        Load auto-calculated geometry from cache.
        Cache file: settings/auto_geometry_cache.json
        """
        cache_file = self.img_path / "settings" / "auto_geometry_cache.json"
        if not cache_file.exists():
            return
        
        try:
            import json
            with open(cache_file) as f:
                cache = json.load(f)
            
            if self.img_name in cache:
                data = cache[self.img_name]
                if 'center' in data and data['center']:
                    self._computed_center = tuple(data['center'])
                if 'rotation' in data and data['rotation'] is not None:
                    self._computed_rotation = data['rotation']
                print(f"Loaded auto geometry from cache: center={self._computed_center}, rotation={self._computed_rotation}")
        except Exception as e:
            print(f"Error loading auto geometry cache: {e}")
    
    def _save_auto_cache(self):
        """
        Save auto-calculated geometry to cache.
        Cache file: settings/auto_geometry_cache.json
        """
        import json
        cache_file = self.img_path / "settings" / "auto_geometry_cache.json"
        
        # Load existing cache
        cache = {}
        if cache_file.exists():
            try:
                with open(cache_file) as f:
                    cache = json.load(f)
            except:
                cache = {}
        
        # Update with current values
        cache[self.img_name] = {
            'center': list(self._computed_center) if self._computed_center else None,
            'rotation': self._computed_rotation if self._computed_rotation is not None else None
        }
        
        # Save
        try:
            cache_file.parent.mkdir(parents=True, exist_ok=True)
            with open(cache_file, 'w') as f:
                json.dump(cache, f, indent=2)
            print(f"Saved auto geometry to cache: {cache[self.img_name]}")
        except Exception as e:
            print(f"Error saving auto geometry cache: {e}")
    
    # ==================== Auto Calculation ====================
    
    def ensure_center(self, force: bool = False) -> Tuple[float, float]:
        """
        Ensure center is available: use manual if set, otherwise calculate.
        
        :param force: Force recalculation even if cached
        :return: center coordinates (x, y)
        """
        # Use manual if available
        if self._manual_center is not None:
            print(f"Using manual center: {self._manual_center}")
            return self._manual_center
        
        # Use cached if available and not forcing
        if not force and self._computed_center is not None:
            print(f"Using cached computed center: {self._computed_center}")
            return self._computed_center
        
        # Calculate new center
        print("Auto-calculating center...")
        img = self._processed_img if self._processed_img is not None else self._raw_img
        
        center = getCenter(img)
        processed_img, center = processImageForIntCenter(img, center)
        
        # Update raw image if we processed it
        if self._processed_img is None:
            self._raw_img = processed_img
        
        self._computed_center = center
        print(f"Center calculated: {center}")
        
        # Save to cache
        self._save_auto_cache()
        
        return center
    
    def ensure_rotation(self, force: bool = False) -> float:
        """
        Ensure rotation is available: use manual if set, otherwise calculate.
        
        :param force: Force recalculation even if cached
        :return: rotation angle in degrees
        """
        # Use manual if available
        if self._manual_rotation is not None:
            print(f"Using manual rotation: {self._manual_rotation}°")
            return self._manual_rotation
        
        # Use cached if available and not forcing
        if not force and self._computed_rotation is not None:
            print(f"Using cached computed rotation: {self._computed_rotation}°")
            return self._computed_rotation
        
        # Ensure center exists first
        center = self.ensure_center()
        
        # Calculate rotation
        print("Auto-calculating rotation...")
        img = self._processed_img if self._processed_img is not None else self._raw_img
        
        if self.detector:
            rotation = getRotationAngle(
                img, center, self.orientation_model, 
                man_det=self.detector
            )
        else:
            rotation = getRotationAngle(img, center, self.orientation_model)
        
        self._computed_rotation = rotation
        print(f"Rotation calculated: {rotation}°")
        
        # Save to cache
        self._save_auto_cache()
        
        return rotation
    
    # ==================== Preprocessing ====================
    
    def _load_blank_and_mask(self):
        """Load blank image and mask from settings directory (lazy loading)"""
        if self._blank_mask_loaded:
            return
        
        blank, mask, weight = getBlankImageAndMask(str(self.img_path), return_weight=True)
        mask_only = getMaskOnly(str(self.img_path))
        
        self._blank_img = blank
        self._mask = mask
        self._mask_only = mask_only
        self.blank_weight = weight
        self._blank_mask_loaded = True
        
        if blank is not None:
            print(f"Loaded blank image with weight {weight}")
        if mask is not None or mask_only is not None:
            print("Loaded mask")
    
    def apply_preprocessing(self, force: bool = False) -> np.ndarray:
        """
        Apply blank image subtraction and mask.
        
        :param force: Force reprocessing even if already applied
        :return: Processed image
        """
        if not force and self._preprocessing_applied and self._processed_img is not None:
            return self._processed_img
        
        img = self._raw_img.copy()
        
        # Skip if nothing to apply
        if not self.apply_blank and not self.apply_mask:
            self._processed_img = img
            self._preprocessing_applied = True
            return img
        
        # Load blank and mask data if needed
        self._load_blank_and_mask()
        
        # Apply blank image subtraction
        if self.apply_blank and self._blank_img is not None:
            img = img - self._blank_img * self.blank_weight
            img = np.clip(img, 0, None)  # No negative values after subtraction
            print(f"Applied blank subtraction (weight: {self.blank_weight})")
        
        # Apply masks
        if self.apply_mask:
            if self._mask is not None:
                img[self._mask == 0] = self.invalid_pixel_threshold
                print("Applied mask")
            if self._mask_only is not None:
                img[self._mask_only == 0] = self.invalid_pixel_threshold
                print("Applied mask-only")
        
        self._processed_img = img
        self._preprocessing_applied = True
        return img
    
    def get_working_image(self) -> np.ndarray:
        """
        Get the image that should be used for processing.
        Applies preprocessing if enabled and not yet applied.
        
        :return: Working image copy (safe to modify)
        """
        if not self._preprocessing_applied:
            self.apply_preprocessing()
        
        if self._processed_img is not None:
            return self._processed_img.copy()
        
        return self._raw_img.copy()
    
    def reset_preprocessing(self):
        """Clear processed image (will be reapplied on next access)"""
        self._processed_img = None
        self._preprocessing_applied = False
    
    # ==================== Fingerprint for Cache Validation ====================
    
    def get_fingerprint(self) -> Dict[str, Any]:
        """
        Generate fingerprint of all factors affecting processing results.
        
        Includes:
        - Config files (blank_image_settings.json, mask.tif, etc.)
        - Manual geometry (center, rotation) if set
        - Preprocessing flags
        
        Returns dict that can be compared for cache invalidation.
        """
        fingerprint = {}
        settings_dir = self.img_path / "settings"
        
        # 1. Track config files (mtime + size)
        config_files = [
            'blank_image_settings.json',
            'mask.tif',
            'mask_config.json',
            '.blank_image_disabled',
            '.mask_disabled'
        ]
        
        for config_file in config_files:
            file_path = settings_dir / config_file
            if file_path.exists():
                stat = file_path.stat()
                fingerprint[f'config:{config_file}'] = {
                    'mtime': stat.st_mtime,
                    'size': stat.st_size
                }
            else:
                fingerprint[f'config:{config_file}'] = None
        
        # 2. Track manual geometry (only if manually set)
        if self._manual_center is not None:
            fingerprint['manual_center'] = self._manual_center
        
        if self._manual_rotation is not None:
            fingerprint['manual_rotation'] = self._manual_rotation
        
        # 3. Track preprocessing flags
        fingerprint['apply_blank'] = self.apply_blank
        fingerprint['apply_mask'] = self.apply_mask
        fingerprint['blank_weight'] = self.blank_weight
        
        return fingerprint
    
    @staticmethod
    def compare_fingerprints(old_fp: Dict, new_fp: Dict) -> Tuple[bool, list]:
        """
        Compare two fingerprints and return what changed.
        
        :param old_fp: Old fingerprint dict
        :param new_fp: New fingerprint dict
        :return: (has_changes, list_of_changes)
        """
        changes = []
        all_keys = set(old_fp.keys()) | set(new_fp.keys())
        
        for key in all_keys:
            old_val = old_fp.get(key)
            new_val = new_fp.get(key)
            
            if old_val != new_val:
                if key.startswith('config:'):
                    # Config file change
                    config_name = key.split(':', 1)[1]
                    if old_val is None:
                        changes.append(f"{config_name} (added)")
                    elif new_val is None:
                        changes.append(f"{config_name} (removed)")
                    else:
                        changes.append(f"{config_name} (modified)")
                else:
                    # Parameter change
                    changes.append(key)
        
        return len(changes) > 0, changes
    
    # ==================== Utility Methods ====================
    
    def __repr__(self) -> str:
        center_str = "Manual" if self._manual_center else ("Auto" if self._computed_center else "None")
        rotation_str = "Manual" if self._manual_rotation else ("Auto" if self._computed_rotation else "None")
        preproc = "Yes" if self._preprocessing_applied else "No"
        return (
            f"ImageData('{self.img_name}', "
            f"shape={self.shape}, "
            f"center={center_str}, "
            f"rotation={rotation_str}, "
            f"preprocessing={preproc})"
        )

