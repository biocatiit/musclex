import os
import json
import time
from datetime import datetime

import numpy as np

CACHE_VERSION = 2


def _finite_float(value, default=0.0):
    try:
        number = float(value)
    except (TypeError, ValueError):
        return float(default)
    return number if np.isfinite(number) else float(default)


def _to_jsonable(value):
    """Convert numpy and complex types to JSON-serializable format."""
    if isinstance(value, np.ndarray):
        return value.tolist()
    if isinstance(value, np.generic):
        return value.item()
    if isinstance(value, dict):
        return {str(key): _to_jsonable(val) for key, val in value.items()}
    if isinstance(value, (list, tuple, set)):
        return [_to_jsonable(item) for item in value]
    return value


def _atomic_write_json(file_path, data):
    """Write data to JSON file atomically using temporary file."""
    directory = os.path.dirname(file_path)
    if directory:
        os.makedirs(directory, exist_ok=True)

    tmp_path = f"{file_path}.tmp"
    try:
        with open(tmp_path, "w", encoding="utf-8") as handle:
            json.dump(_to_jsonable(data), handle, ensure_ascii=False, indent=2, sort_keys=True)
        os.replace(tmp_path, file_path)
    except Exception:
        if os.path.exists(tmp_path):
            os.remove(tmp_path)
        raise


def load_optimization_cache(file_path):
    """Load optimization cache from file with validation."""
    default_cache = {"version": CACHE_VERSION, "entries": {}}
    
    if not os.path.exists(file_path):
        return default_cache
    
    try:
        with open(file_path, "r", encoding="utf-8") as handle:
            data = json.load(handle)
    except Exception:
        return default_cache

    # Validate structure
    if not isinstance(data, dict):
        return default_cache
    
    data.setdefault("version", CACHE_VERSION)
    if not isinstance(data.get("entries"), dict):
        data["entries"] = {}
    
    return data


def _ensure_entry_defaults(entry):
    """Ensure entry has required structure with defaults."""
    if not isinstance(entry, dict):
        entry = {}
    entry.setdefault("user_background_configurations", [])
    return entry

def _normalize_dataset_key(cache_key):
    """Normalize cache key to dataset identifier."""
    dataset_key = str(cache_key or "").strip()
    return dataset_key or "__default_dataset__"

def _sanitize_user_configuration(item):
    """Validate and sanitize user configuration item."""
    if not isinstance(item, dict):
        return None

    method = item.get("method")
    if method is None:
        return None

    # Extract and validate params
    params = item.get("params", {})
    if not isinstance(params, dict):
        params = {}
    
    # Get defaults from params
    params_downsample = params.get("downsample", 1)
    params_smooth_image = params.get("smooth_image", False)

    # Sanitize loss value
    loss_val = item.get("loss")
    if loss_val is not None:
        loss_num = _finite_float(loss_val, float("nan"))
        loss_val = None if np.isnan(loss_num) else float(loss_num)

    # Build sanitized config
    cleaned = {
        "name": str(item.get("name", "")).strip(),
        "method": str(method),
        "params": dict(params),
        "mode": str(item.get("mode", "")).strip(),
        "loss": loss_val,
        "downsample": item.get("downsample", params_downsample),
        "smooth_image": item.get("smooth_image", params_smooth_image),
        "additional_info": item.get("additional_info", {}),
    }

    return cleaned

def _make_configuration_key(method, params, downsample=1, smooth_image=False):
    payload = {
        "method": str(method),
        "params": _to_jsonable(dict(params or {})),
        "downsample": downsample,
        "smooth_image": smooth_image,
    }
    return json.dumps(payload, sort_keys=True)


def get_user_background_configurations(file_path, cache_key):
    """Retrieve user background configurations, deduplicating by config key."""
    cache = load_optimization_cache(file_path)
    dataset_key = _normalize_dataset_key(cache_key)
    entry = cache.get("entries", {}).get(dataset_key)
    
    if not entry:
        return []

    entry = _ensure_entry_defaults(entry)
    user_configs = entry.get("user_background_configurations", [])
    if not isinstance(user_configs, list):
        return []

    rows = []
    seen_by_config_key = set()
    
    for item in user_configs:
        clean_item = _sanitize_user_configuration(item)
        if clean_item is None:
            continue

        cfg_key = _make_configuration_key(
            clean_item["method"],
            clean_item["params"],
            downsample=clean_item["downsample"],
            smooth_image=clean_item["smooth_image"],
        )
        
        if cfg_key not in seen_by_config_key:
            seen_by_config_key.add(cfg_key)
            rows.append(clean_item)

    return rows



def set_user_background_configurations(file_path, cache_key, configurations, additional_info=None):
    """Set user background configurations, deduplicating by name."""
    cache = load_optimization_cache(file_path)
    entries = cache.setdefault("entries", {})

    dataset_key = _normalize_dataset_key(cache_key)
    entry = _ensure_entry_defaults(entries.get(dataset_key, {}))

    # Convert additional_info once if provided
    additional_info_str = ""
    if isinstance(additional_info, dict) and additional_info:
        additional_info_str = json.dumps(_to_jsonable(additional_info), sort_keys=True)

    cleaned_rows = []
    seen_names = set()
    now_text = str(datetime.fromtimestamp(time.time()))
    
    for item in configurations or []:
        clean_item = _sanitize_user_configuration(item)
        if clean_item is None:
            continue

        # Deduplicate by name - keep most-recent entry per name
        name_key = clean_item["name"]
        if name_key and name_key in seen_names:
            continue
        if name_key:
            seen_names.add(name_key)

        if additional_info_str:
            clean_item["additional_info"] = additional_info_str

        cleaned_rows.append(clean_item)

    entry["user_background_configurations"] = cleaned_rows
    entry["dataset"] = dataset_key
    entry["updated_at"] = now_text
    entries[dataset_key] = entry
    
    _atomic_write_json(file_path, cache)
