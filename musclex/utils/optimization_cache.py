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
    if isinstance(value, dict):
        return {str(key): _to_jsonable(val) for key, val in value.items()}
    if isinstance(value, (list, tuple, set)):
        return [_to_jsonable(item) for item in value]
    if isinstance(value, np.generic):
        return value.item()
    if isinstance(value, np.ndarray):
        return value.tolist()
    return value


def _atomic_write_json(file_path, data):
    directory = os.path.dirname(file_path)
    if directory:
        os.makedirs(directory, exist_ok=True)

    tmp_path = f"{file_path}.tmp"
    with open(tmp_path, "w", encoding="utf-8") as handle:
        json.dump(_to_jsonable(data), handle, ensure_ascii=False, indent=2, sort_keys=True)
    os.replace(tmp_path, file_path)


def delete_optimization_cache(file_path):
    if os.path.exists(file_path):
        try:
            os.remove(file_path)
        except Exception:
            pass

def load_optimization_cache(file_path):
    if not os.path.exists(file_path):
        return {"version": CACHE_VERSION, "entries": {}}

    try:
        with open(file_path, "r", encoding="utf-8") as handle:
            data = json.load(handle)
    except Exception:
        return {"version": CACHE_VERSION, "entries": {}}

    if not isinstance(data, dict):
        return {"version": CACHE_VERSION, "entries": {}}

    data.setdefault("version", CACHE_VERSION)
    if not isinstance(data.get("entries"), dict):
        data["entries"] = {}
    return data


def _ensure_entry_defaults(entry):
    if not isinstance(entry, dict):
        entry = {}
    if not isinstance(entry.get("configurations"), dict):
        entry["configurations"] = {}
    if not isinstance(entry.get("user_background_configurations"), list):
        entry["user_background_configurations"] = []
    return entry

def _normalize_dataset_key(cache_key):
    dataset_key = str(cache_key or "").strip()
    return dataset_key if dataset_key else "__default_dataset__"

def _sanitize_downsample(value, default=1):
    try:
        number = int(value)
    except (TypeError, ValueError):
        number = int(default)
    return max(1, number)

def _sanitize_smooth_image(value, default=False):
    if value is None:
        return bool(default)
    if isinstance(value, str):
        return value.strip().lower() in {"1", "true", "yes", "y", "on"}
    return bool(value)

def _sanitize_additional_info(info):
    if not isinstance(info, dict):
        return {}
    return _to_jsonable(dict(info))

def _info_signature(info):
    if not isinstance(info, dict) or len(info) == 0:
        return None
    return json.dumps(_to_jsonable(info), sort_keys=True)

def _matches_additional_info(item_info, expected_info):
    expected_sig = _info_signature(expected_info)
    if expected_sig is None:
        return True

    # Backward compatibility for older rows without context information.
    item_sig = _info_signature(item_info)
    if item_sig is None:
        return True
    return item_sig == expected_sig


def _sanitize_user_configuration(item):
    if not isinstance(item, dict):
        return None

    name = str(item.get("name", "")).strip()
    method = item.get("method")
    params = item.get("params", {})
    if method is None:
        return None
    if not isinstance(params, dict):
        params = {}

    params_downsample = params.get("downsample", params.get("_downsample", 1))
    params_smooth_image = params.get("smooth_image", params.get("_smooth_image", False))

    loss_val = item.get("loss", None)
    if loss_val is not None:
        loss_num = _finite_float(loss_val, float("nan"))
        loss_val = None if np.isnan(loss_num) else float(loss_num)

    cleaned = {
        "name": name,
        "method": str(method),
        "params": dict(params),
        "mode": str(item.get("mode", "")) if item.get("mode", "") is not None else "",
        "loss": loss_val,
        "downsample": _sanitize_downsample(item.get("downsample", params_downsample), 1),
        "smooth_image": _sanitize_smooth_image(item.get("smooth_image", params_smooth_image), False),
        "additional_info": _sanitize_additional_info(item.get("additional_info", {})),
    }

    if item.get("created_at") is not None:
        cleaned["created_at"] = str(item.get("created_at"))
    if item.get("updated_at") is not None:
        cleaned["updated_at"] = str(item.get("updated_at"))

    return cleaned

def _make_configuration_key(method, params, downsample=1, smooth_image=False):
    payload = {
        "method": str(method),
        "params": _to_jsonable(dict(params or {})),
        "downsample": _sanitize_downsample(downsample, 1),
        "smooth_image": _sanitize_smooth_image(smooth_image, False),
    }
    return json.dumps(payload, sort_keys=True)


def _ensure_configuration_defaults(configuration, method=None, params=None, downsample=1, smooth_image=False, additional_info=None):
    if configuration is None:
        configuration = {}

    configuration["method"] = str(configuration.get("method", method)) if configuration.get("method", method) is not None else None
    configuration["params"] = dict(configuration.get("params") or dict(params or {}))
    configuration["downsample"] = _sanitize_downsample(configuration.get("downsample", downsample), 1)
    configuration["smooth_image"] = _sanitize_smooth_image(configuration.get("smooth_image", smooth_image), False)
    configuration["additional_info"] = _sanitize_additional_info(configuration.get("additional_info", additional_info or {}))

    try:
        n = int(configuration.get("n", 0))
    except (TypeError, ValueError):
        n = 0
    if n < 0:
        n = 0

    cumulative_loss = _finite_float(configuration.get("cumulative_loss", 0.0), 0.0)
    avg_loss = configuration.get("avg_loss")
    if avg_loss is None:
        avg_loss = (cumulative_loss / n) if n > 0 else 0.0
    avg_loss = _finite_float(avg_loss, 0.0)

    loss_m2 = _finite_float(configuration.get("loss_m2", 0.0), 0.0)
    if "std_loss" in configuration:
        std_loss = _finite_float(configuration.get("std_loss", 0.0), 0.0)
    else:
        std_loss = float(np.sqrt(loss_m2 / n)) if n > 0 else 0.0

    configuration["n"] = n
    configuration["cumulative_loss"] = cumulative_loss
    configuration["avg_loss"] = avg_loss
    configuration["loss_m2"] = loss_m2
    configuration["std_loss"] = std_loss
    configuration["best_loss"] = _finite_float(configuration.get("best_loss", float("inf")), float("inf"))
    configuration["last_loss"] = _finite_float(configuration.get("last_loss", float("inf")), float("inf"))
    configuration.setdefault("updated_at", None)
    return configuration


def _record_configuration_result(entry, method, params, loss, downsample=1, smooth_image=False, additional_info=None):
    loss = _finite_float(loss, float("nan"))
    if np.isnan(loss):
        return entry

    configurations = entry.setdefault("configurations", {})
    key = _make_configuration_key(method, params, downsample=downsample, smooth_image=smooth_image)
    configuration = _ensure_configuration_defaults(
        configurations.get(key),
        method=method,
        params=params,
        downsample=downsample,
        smooth_image=smooth_image,
        additional_info=additional_info,
    )

    n_old = int(configuration.get("n", 0))
    n_new = n_old + 1
    mean_old = _finite_float(configuration.get("avg_loss", 0.0), 0.0)
    m2_old = _finite_float(configuration.get("loss_m2", 0.0), 0.0)

    delta = loss - mean_old
    mean_new = mean_old + (delta / n_new)
    delta2 = loss - mean_new
    m2_new = m2_old + (delta * delta2)

    configuration["n"] = n_new
    configuration["cumulative_loss"] = _finite_float(configuration.get("cumulative_loss", 0.0), 0.0) + loss
    configuration["avg_loss"] = mean_new
    configuration["loss_m2"] = m2_new
    configuration["std_loss"] = float(np.sqrt(m2_new / n_new)) if n_new > 0 else 0.0
    configuration["best_loss"] = min(_finite_float(configuration.get("best_loss", float("inf")), float("inf")), loss)
    configuration["last_loss"] = loss
    configuration["method"] = str(method)
    configuration["params"] = dict(params or {})
    configuration["downsample"] = _sanitize_downsample(downsample, 1)
    configuration["smooth_image"] = _sanitize_smooth_image(smooth_image, False)
    configuration["additional_info"] = _sanitize_additional_info(additional_info or {})
    configuration["updated_at"] = str(datetime.fromtimestamp(time.time()))

    configurations[key] = configuration
    return entry


def _record_loss(entry, loss):
    loss = _finite_float(loss, float("nan"))
    if np.isnan(loss):
        return entry

    n_old = int(entry.get("n", 0))
    n_new = n_old + 1

    mean_old = _finite_float(entry.get("loss_mean", 0.0), 0.0)
    m2_old = _finite_float(entry.get("loss_m2", 0.0), 0.0)

    delta = loss - mean_old
    mean_new = mean_old + (delta / n_new)
    delta2 = loss - mean_new
    m2_new = m2_old + (delta * delta2)

    entry["n"] = n_new
    entry["cumulative_loss"] = _finite_float(entry.get("cumulative_loss", 0.0), 0.0) + loss
    entry["loss_mean"] = mean_new
    entry["loss_m2"] = m2_new
    entry["loss_std"] = float(np.sqrt(m2_new / n_new)) if n_new > 0 else 0.0
    return entry


def _update_top_two(entry, method, params, loss, downsample=1, smooth_image=False):
    loss = _finite_float(loss, float("nan"))
    if np.isnan(loss):
        return entry

    params = dict(params or {})
    configuration_key = _make_configuration_key(method, params, downsample=downsample, smooth_image=smooth_image)

    best_loss = _finite_float(entry.get("best_loss", float("inf")), float("inf"))
    second_best_loss = _finite_float(entry.get("second_best_loss", float("inf")), float("inf"))
    best_key = entry.get("best_configuration_key")
    second_key = entry.get("second_best_configuration_key")

    if configuration_key == best_key:
        if loss < best_loss:
            entry["best_loss"] = float(loss)
    elif loss < best_loss:
        entry["second_best_loss"] = best_loss
        entry["second_best_method"] = entry.get("best_method")
        entry["second_best_params"] = dict(entry.get("best_params") or {})
        entry["second_best_downsample"] = entry.get("best_downsample")
        entry["second_best_smooth_image"] = entry.get("best_smooth_image")
        entry["second_best_configuration_key"] = best_key

        entry["best_loss"] = float(loss)
        entry["best_method"] = method
        entry["best_params"] = params
        entry["best_downsample"] = _sanitize_downsample(downsample, 1)
        entry["best_smooth_image"] = _sanitize_smooth_image(smooth_image, False)
        entry["best_configuration_key"] = configuration_key
    elif configuration_key == second_key:
        if loss < second_best_loss:
            entry["second_best_loss"] = float(loss)
    elif loss < second_best_loss:
        entry["second_best_loss"] = float(loss)
        entry["second_best_method"] = method
        entry["second_best_params"] = params
        entry["second_best_downsample"] = _sanitize_downsample(downsample, 1)
        entry["second_best_smooth_image"] = _sanitize_smooth_image(smooth_image, False)
        entry["second_best_configuration_key"] = configuration_key

    return entry


def get_cached_loss_statistics(file_path, cache_key, min_samples=3):
    cache = load_optimization_cache(file_path)
    dataset_key = _normalize_dataset_key(cache_key)
    entry = cache.get("entries", {}).get(dataset_key)
    if not entry:
        return None

    entry = _ensure_entry_defaults(entry)
    n = int(entry.get("n", 0))
    if n < int(min_samples):
        return None

    return {
        "n": n,
        "avg_loss": float(entry.get("loss_mean", 0.0)),
        "std_loss": float(entry.get("loss_std", 0.0)),
    }


def get_user_background_configurations(file_path, cache_key, additional_info=None):
    cache = load_optimization_cache(file_path)
    dataset_key = _normalize_dataset_key(cache_key)
    entry = cache.get("entries", {}).get(dataset_key)
    if not entry:
        return []

    entry = _ensure_entry_defaults(entry)
    user_configs = entry.get("user_background_configurations", [])
    if not isinstance(user_configs, list):
        user_configs = []

    rows = []
    seen_by_config_key = set()
    for item in user_configs:
        clean_item = _sanitize_user_configuration(item)
        if clean_item is None:
            continue

        # if not _matches_additional_info(clean_item.get("additional_info"), additional_info):
        #     continue

        cfg_key = _make_configuration_key(
            clean_item.get("method"),
            clean_item.get("params", {}),
            downsample=clean_item.get("downsample", 1),
            smooth_image=clean_item.get("smooth_image", False),
        )
        if cfg_key in seen_by_config_key:
            continue
        seen_by_config_key.add(cfg_key)
        rows.append(clean_item)

    return rows


def set_user_background_configurations(file_path, cache_key, configurations, additional_info=None):
    cache = load_optimization_cache(file_path)
    entries = cache.setdefault("entries", {})

    dataset_key = _normalize_dataset_key(cache_key)
    additional_info = _sanitize_additional_info(additional_info)
    entry = entries.get(dataset_key)
    entry = _ensure_entry_defaults(entry)

    cleaned_rows = []
    seen_names = set()
    now_text = str(datetime.fromtimestamp(time.time()))
    for item in configurations or []:
        clean_item = _sanitize_user_configuration(item)
        if clean_item is None:
            continue

        if _info_signature(clean_item.get("additional_info")) is None:
            clean_item["additional_info"] = dict(additional_info)

        # Keep most-recent entry per name, if provided.
        name_key = clean_item.get("name", "")
        if name_key and name_key in seen_names:
            continue
        if name_key:
            seen_names.add(name_key)

        clean_item.setdefault("created_at", now_text)
        clean_item["updated_at"] = now_text
        cleaned_rows.append(clean_item)

    entry["user_background_configurations"] = cleaned_rows
    # From this point, configurations are managed as user configurations.

    entry["dataset"] = dataset_key
    entry["updated_at"] = now_text
    entries[dataset_key] = entry
    _atomic_write_json(file_path, cache)




def optimization_cache_writer(queue):
    caches_by_path = {}

    while True:
        item = queue.get()
        if item is None:
            break
        if not isinstance(item, dict):
            continue
        if item.get("type") != "result":
            continue

        cache_path = item.get("cache_path")
        cache_key = item.get("cache_key")
        method = item.get("method")
        params = item.get("params")
        loss = item.get("loss")
        additional_info = _sanitize_additional_info(item.get("additional_info", {}))
        downsample = _sanitize_downsample(item.get("downsample", item.get("downsample_factor", 1)), 1)
        smooth_image = _sanitize_smooth_image(item.get("smooth_image", False), False)

        if not cache_path or cache_key is None or method is None or params is None or loss is None:
            continue
        if np.isnan(_finite_float(loss, float("nan"))):
            continue

        cache = caches_by_path.get(cache_path)
        if cache is None:
            cache = load_optimization_cache(cache_path)
            caches_by_path[cache_path] = cache

        entries = cache.setdefault("entries", {})
        dataset_key = _normalize_dataset_key(cache_key)
        entry = entries.get(dataset_key)
        entry = _ensure_entry_defaults(entry)
        entry = _record_loss(entry, loss)
        entry = _record_configuration_result(
            entry,
            method,
            params,
            loss,
            downsample=downsample,
            smooth_image=smooth_image,
            additional_info=additional_info,
        )
        entry = _update_top_two(entry, method, params, loss, downsample=downsample, smooth_image=smooth_image)

        entry["updated_at"] = str(datetime.fromtimestamp(time.time()))
        entries[dataset_key] = entry

        try:
            _atomic_write_json(cache_path, cache)
        except Exception:
            pass
