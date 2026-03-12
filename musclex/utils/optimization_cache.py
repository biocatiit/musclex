import os
import json
import time
from datetime import datetime

import numpy as np

CACHE_VERSION = 1


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
    data.setdefault("entries", {})
    return data


def _ensure_entry_defaults(entry):
    if entry is None:
        entry = {}

    try:
        n = int(entry.get("n", 0))
    except (TypeError, ValueError):
        n = 0
    if n < 0:
        n = 0

    cumulative_loss = _finite_float(entry.get("cumulative_loss", 0.0), 0.0)
    loss_mean = entry.get("loss_mean")
    if loss_mean is None:
        loss_mean = (cumulative_loss / n) if n > 0 else 0.0
    loss_mean = _finite_float(loss_mean, 0.0)

    loss_m2 = _finite_float(entry.get("loss_m2", 0.0), 0.0)
    if "loss_std" in entry:
        loss_std = _finite_float(entry.get("loss_std", 0.0), 0.0)
    else:
        loss_std = float(np.sqrt(loss_m2 / n)) if n > 0 else 0.0

    entry["best_loss"] = _finite_float(entry.get("best_loss", float("inf")), float("inf"))
    entry.setdefault("best_method", None)
    entry.setdefault("best_params", {})
    entry.setdefault("updated_at", None)

    entry["second_best_loss"] = _finite_float(entry.get("second_best_loss", float("inf")), float("inf"))
    entry.setdefault("second_best_method", None)
    entry.setdefault("second_best_params", {})

    entry["n"] = n
    entry["cumulative_loss"] = cumulative_loss
    entry["loss_mean"] = loss_mean
    entry["loss_m2"] = loss_m2
    entry["loss_std"] = loss_std
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
    # entry["best_loss"] = loss if loss < float(entry.get("best_loss", float("inf"))) else float(entry.get("best_loss", float("inf")))
    # entry["updated_at"] = str(datetime.fromtimestamp(time.time()))
    return entry


def _update_top_two(entry, method, params, loss):
    loss = _finite_float(loss, float("nan"))
    if np.isnan(loss):
        return entry

    params = dict(params or {})
    best_loss = _finite_float(entry.get("best_loss", float("inf")), float("inf"))
    second_best_loss = _finite_float(entry.get("second_best_loss", float("inf")), float("inf"))

    best_method = entry.get("best_method")
    best_params = dict(entry.get("best_params") or {})
    second_best_method = entry.get("second_best_method")
    second_best_params = dict(entry.get("second_best_params") or {})

    is_same_as_best = (method == best_method and params == best_params)
    is_same_as_second = (method == second_best_method and params == second_best_params)

    if is_same_as_best:
        if loss < best_loss:
            entry["best_loss"] = float(loss)
    elif loss < best_loss:
        # Promote old best to second-best only when it represents a different candidate.
        entry["second_best_loss"] = best_loss
        entry["second_best_method"] = best_method
        entry["second_best_params"] = best_params

        entry["best_loss"] = float(loss)
        entry["best_method"] = method
        entry["best_params"] = params
    elif is_same_as_second:
        if loss < second_best_loss:
            entry["second_best_loss"] = float(loss)
    elif loss < second_best_loss:
        entry["second_best_loss"] = float(loss)
        entry["second_best_method"] = method
        entry["second_best_params"] = params

    return entry


def get_cached_loss_statistics(file_path, cache_key, min_samples=3):
    cache = load_optimization_cache(file_path)
    entry = cache.get("entries", {}).get(cache_key)
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


def should_rerun_optimization(file_path, cache_key, loss, min_samples=3, stddev_multiplier=1.0):
    stats = get_cached_loss_statistics(file_path, cache_key, min_samples=min_samples)
    if stats is None:
        return False, None

    threshold = float(stats["avg_loss"]) + float(stddev_multiplier) * float(stats["std_loss"])
    decision = float(loss) > threshold
    stats_with_threshold = dict(stats)
    stats_with_threshold["threshold_loss"] = threshold
    return decision, stats_with_threshold


def get_cached_best_result(file_path, cache_key, min_samples=3):
    cache = load_optimization_cache(file_path)
    entry = cache.get("entries", {}).get(cache_key)
    if not entry:
        return None

    entry = _ensure_entry_defaults(entry)

    if entry.get("n", 0) < int(min_samples):
        return None

    if entry.get("best_method") is None or entry.get("best_params") is None:
        return None

    return {
        "best_method": entry["best_method"],
        "best_params": entry["best_params"],
        "best_loss": entry.get("best_loss"),
        "second_best_loss": entry.get("second_best_loss"),
        "second_best_method": entry.get("second_best_method"),
        "second_best_params": entry.get("second_best_params"),
        "avg_loss": entry.get("loss_mean"),
        "std_loss": entry.get("loss_std"),
        "n": entry.get("n", 0),
    }


def update_optimization_cache(file_path, cache_key, method, params, loss):
    if np.isnan(_finite_float(loss, float("nan"))):
        return

    cache = load_optimization_cache(file_path)
    entries = cache.setdefault("entries", {})

    entry = entries.get(cache_key)
    entry = _ensure_entry_defaults(entry)
    entry = _record_loss(entry, loss)
    entry = _update_top_two(entry, method, params, loss)

    entry["updated_at"] = str(datetime.fromtimestamp(time.time()))
    entries[cache_key] = entry
    _atomic_write_json(file_path, cache)


def publish_optimization_result(queue, payload):
    if queue is None:
        return

    message = dict(payload)
    message["type"] = "result"
    queue.put(message)


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

        if not cache_path or cache_key is None or method is None or params is None or loss is None:
            continue
        if np.isnan(_finite_float(loss, float("nan"))):
            continue

        cache = caches_by_path.get(cache_path)
        if cache is None:
            cache = load_optimization_cache(cache_path)
            caches_by_path[cache_path] = cache

        entries = cache.setdefault("entries", {})
        entry = entries.get(cache_key)
        entry = _ensure_entry_defaults(entry)
        entry = _record_loss(entry, loss)
        entry = _update_top_two(entry, method, params, loss)

        entry["updated_at"] = str(datetime.fromtimestamp(time.time()))
        entries[cache_key] = entry

        try:
            _atomic_write_json(cache_path, cache)
        except Exception:
            pass
