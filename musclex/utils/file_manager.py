"""
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
import traceback
from datetime import datetime
from os.path import split, exists, join
import numpy as np
import fabio
#from ..ui.pyqt_utils import *
from .hdf5_manager import loadFile
from PySide6.QtWidgets import QMessageBox
from concurrent.futures import ProcessPoolExecutor
import hashlib
import time
import pickle

input_types = ['adsc', 'cbf', 'edf', 'fit2d', 'mar345', 'marccd', 'hdf5', 'h5', 'pilatus', 'tif', 'tiff', 'smv']

def getFilesAndHdf(dir_path):
    """
    Give the image files and hdf files in a folder selected
    :param dir_path: directory path
    :return: image list, hdf list
    """
    fileList = os.listdir(dir_path)
    imgList = []
    hdfList = []

    for f in fileList:
        full_file_name = fullPath(dir_path, f)
        if isImg(full_file_name):
            imgList.append(f)
        else:
            toks = f.split('.')
            if toks[-1] == 'hdf':
                hdfList.append(f)

    return imgList, hdfList

def getImgFiles(fullname, headless=False):
    """
    Get directory, all image file names in the same directory and current file index
    :param fullname: full name of the file including directory i.e. /aaa/bbb/ccc/ddd.tif (str)
    :return: directory (str), list of image file names, and current index i.e /aaa/bbb/ccc, ["ddd.tif","eee.tif"], 0
    """
    dir_path, filename = split(str(fullname)) # split directory and file name from full file name
    dir_path = str(dir_path)
    filename = str(filename)
    _, ext = os.path.splitext(str(filename))
    current = 0
    failedcases = []
    filename_index = None

    if ext == ".txt":
        for line in open(fullname, "r"):
            failedcases.append(line.rstrip('\n'))
    else:
        failedcases = None

    if ext in ('.hdf5', '.h5'):
        fileList = loadFile(fullname)
        imgList = []
        if fileList is None or not fileList or None in fileList:
            infMsg = QMessageBox()
            infMsg.setText('Error opening file: ' + fullname)
            infMsg.setInformativeText("File is not a valid HDF5 file or corrupted.")
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
            return None, None, None, None, None
        for f in fileList[0]:
            if failedcases is not None and f not in failedcases:
                continue
            imgList.append(f)
        if len(imgList) == 1 and not headless:
            # if only one image in the h5 file, take all the single h5 images in the folder
            infMsg = QMessageBox()
            infMsg.setText('Single Image H5 File')
            infMsg.setInformativeText("The H5 file selected contains only one image. All the H5 files in the current folder containing only one image will be regrouped the same way as a folder containing TIF files.\n")
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
            list_h5_files = os.listdir(dir_path)
            imgList = []
            fileList = [[],[]]
            for f in list_h5_files:
                _, ext2 = os.path.splitext(str(f))
                full_file_name = fullPath(dir_path, f)
                if ext2 in ('.hdf5', '.h5'):
                    file_loader = loadFile(full_file_name)
                    if file_loader[0] is None:
                        infMsg = QMessageBox()
                        infMsg.setText('Error opening file: ' + f)
                        infMsg.setInformativeText("File is not a valid HDF5 file, is corrupted, or is an empty HDF5 Master file.  Skipping.")
                        infMsg.setStandardButtons(QMessageBox.Ok)
                        infMsg.setIcon(QMessageBox.Information)
                        infMsg.exec_()
                        continue
                    if len(file_loader[0]) == 1:
                        if failedcases is not None and file_loader[0][0] not in failedcases:
                            continue
                        imgList.append(file_loader[0][0])
                        fileList[0].append(file_loader[0][0])
                        fileList[1].append(file_loader[1][0])
                        if full_file_name == fullname:
                            filename_index = file_loader[0][0]
            imgList.sort()
    else:
        fileList = os.listdir(dir_path)
        imgList = []
        for f in fileList:
            if failedcases is not None and f not in failedcases:
                continue
            full_file_name = fullPath(dir_path, f)
            _, ext2 = os.path.splitext(str(f))
            if isImg(full_file_name) and f != "calibration.tif" and ext2 not in ('.hdf5', '.h5'):  #and validateImage(full_file_name):
                imgList.append(f)
        imgList.sort()

    if failedcases is None and imgList:
        if ext in ('.hdf5', '.h5'):
            if filename_index is None:
                current = 0
            else:
                current = imgList.index(filename_index)
        else:
            current = imgList.index(filename)
    
    return dir_path, imgList, current, fileList, ext

def fullPath(filePath, fileName):
    """
    Combine a path and file name to get full file name
    :param filePath: directory (string)
    :param fileName: file name (string)
    :return: filePath/filename (string)
    """
    # if filePath[-1] == '/':
    #     return filePath+fileName
    # else:
    #     return filePath+"/"+fileName
    return os.path.join(filePath, fileName)

def isImg(fileName):
    """
    Check if a file name is an image file
    :param fileName: (str)
    :return: True or False
    """
    nameList = fileName.split('.')
    return nameList[-1] in input_types

def validateImage(fileName, showDialog=True):
    try:
        test = fabio.open(fileName).data
        return True
    except Exception:
        if showDialog:
            infMsg = QMessageBox()
            infMsg.setText('Error opening file: ' + fileName)
            infMsg.setInformativeText("Fabio could not open .TIFF File. File is either corrupt or invalid.")
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
        return False

def isHdf5(fileName):
    """
    Check if a file name is an hdf5 file
    :param fileName: (str)
    :return: True or False
    """
    nameList = fileName.split('.')
    return nameList[-1] in ('hdf5', 'h5')

def ifHdfReadConvertless(fileName, img):
    """
    Check if a file name is an hdf5 file
    and convert it to be directly readable without converting to tiff
    :param fileName, img: (str), (array)
    :return: img converted
    """
    if isHdf5(fileName):
        img = img.astype(np.int32)
        img[img==4294967295] = -1
    return img

def createFolder(path):
    """
    Create a folder if it doesn't exist
    :param path: full path of creating directory
    :return:
    """
    if not exists(path):
        os.makedirs(path)

# --------------------- Fast, cached, multiprocessing directory scan ---------------------
_SCAN_CACHE = {}

def _disk_cache_dir(dir_path):
    try:
        return join(dir_path, ".musclex_cache")
    except Exception:
        return None

def _disk_cache_file(dir_path):
    try:
        cdir = _disk_cache_dir(dir_path)
        if cdir is None:
            return None
        # v2: specs stored as relative paths so cache is portable across machines/users.
        # Old scan_cache.pkl (absolute paths) is intentionally ignored.
        return join(cdir, "scan_cache_v2.pkl")
    except Exception:
        return None


def _specs_to_relative(specs):
    """Convert absolute paths in specs to basenames for portable disk storage."""
    result = []
    for spec in specs:
        if isinstance(spec, tuple) and len(spec) >= 2:
            rel = os.path.basename(spec[1])
            result.append((spec[0], rel) + spec[2:])
        else:
            result.append(spec)
    return result


def _specs_to_absolute(dir_path, specs):
    """Restore absolute paths in specs loaded from disk cache."""
    result = []
    for spec in specs:
        if isinstance(spec, tuple) and len(spec) >= 2:
            abs_path = join(dir_path, spec[1])
            result.append((spec[0], abs_path) + spec[2:])
        else:
            result.append(spec)
    return result


def _load_scan_cache_from_disk(dir_path, sig, fallback_dir=None):
    """Try loading scan cache from *dir_path*; fall back to *fallback_dir*."""
    for d in (dir_path, fallback_dir):
        if d is None:
            continue
        try:
            cfile = _disk_cache_file(d)
            if cfile and exists(cfile):
                with open(cfile, "rb") as f:
                    data = pickle.load(f)
                if isinstance(data, dict) and data.get("sig") == sig and isinstance(data.get("payload"), tuple):
                    payload = data.get("payload")
                    if payload and len(payload) >= 2:
                        payload = (payload[0], _specs_to_absolute(dir_path, payload[1])) + payload[2:]
                    return payload
        except Exception:
            pass
    return None

def _save_scan_cache_to_disk(dir_path, sig, payload, fallback_dir=None):
    """Try saving scan cache to *dir_path*; fall back to *fallback_dir*."""
    rel_payload = payload
    if rel_payload and len(rel_payload) >= 2:
        rel_payload = (rel_payload[0], _specs_to_relative(rel_payload[1])) + rel_payload[2:]
    for d in (dir_path, fallback_dir):
        if d is None:
            continue
        try:
            cdir = _disk_cache_dir(d)
            if cdir and not exists(cdir):
                os.makedirs(cdir, exist_ok=True)
            cfile = _disk_cache_file(d)
            if cfile:
                with open(cfile, "wb") as f:
                    pickle.dump({"sig": sig, "payload": rel_payload}, f)
                return  # written successfully
        except Exception:
            continue  # try fallback

def _dir_signature(dir_path):
    try:
        entries = []
        with os.scandir(dir_path) as it:
            for e in it:
                if e.is_file():
                    try:
                        stat = e.stat()
                        entries.append((e.name, stat.st_size, int(stat.st_mtime)))
                    except Exception:
                        # best-effort; skip entries we cannot stat
                        continue
        entries.sort()
        h = hashlib.sha256()
        for name, sz, mt in entries:
            h.update(name.encode('utf-8', errors='ignore'))
            h.update(str(sz).encode())
            h.update(str(mt).encode())
        return h.hexdigest()
    except Exception:
        return None

def _write_error_log(dir_path, error_msg, exception=None, fallback_dir=None):
    """Write a timestamped error entry to musclex_error.log. Safe to call from any thread or subprocess.

    Tries *dir_path* first; falls back to *fallback_dir* if the write fails
    (e.g. when the input directory is read-only).
    """
    ts = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    for d in (dir_path, fallback_dir):
        if d is None:
            continue
        try:
            log_path = os.path.join(d, "musclex_error.log")
            with open(log_path, "a", encoding="utf-8") as lf:
                lf.write(f"[{ts}] {error_msg}\n")
                if exception is not None:
                    tb = "".join(traceback.format_exception(type(exception), exception, exception.__traceback__))
                    lf.write(tb)
                lf.write("\n")
            return
        except Exception:
            continue

def _h5_nframes(path):
    """Return (nframes, (height, width)) for an HDF5 file, or (0, None) on error."""
    try:
        f = fabio.open(path)
        n = getattr(f, 'nframes', 1)
        shape = None
        try:
            data = f.data
            if data is not None:
                shape = data.shape[:2]  # (height, width)
        except Exception:
            pass
        try:
            f.close()
        except Exception:
            pass
        return n, shape
    except Exception as e:
        _write_error_log(
            os.path.dirname(path),
            f"Could not open HDF5 file (skipped during scan): {path}",
            e
        )
        return 0, None

def _tiff_size(path):
    """Return (height, width) for a TIFF/image file by reading only the header. Returns None on error."""
    try:
        from PIL import Image as _PILImage
        with _PILImage.open(path) as im:
            w, h = im.size  # PIL lazy-loads: only header is read
            return (h, w)
    except Exception:
        pass
    try:
        # Fallback: fabio header only (some formats expose shape without .data)
        f = fabio.open(path)
        if hasattr(f, 'shape') and f.shape:
            return f.shape[:2]
        try:
            f.close()
        except Exception:
            pass
    except Exception:
        pass
    return None


def scan_directory_images_cached(dir_path, max_workers=None, progress_dict=None, fallback_cache_dir=None):
    """
    Scan a directory for TIFF and HDF5 images and return unified
    (imgList, loader_specs, source_index_map, size_map).
    Uses a cache keyed by directory content signature. HDF5 frame counts are computed
    in parallel using processes. Frames are NOT loaded.

    NOTE: This function always returns the COMPLETE list of images in the directory.
    Filtering by failedcases should be done by the caller after getting the full list.

    Args:
        max_workers: Number of workers for parallel HDF5 frame counting
        progress_dict: Optional dict to track progress. Will set 'h5_total' and 'h5_done'
        fallback_cache_dir: If the input dir is read-only, try this dir for cache I/O

    Returns:
        imgList:          List of ALL display names (complete list)
        specs:            List of ALL loader specs (tuples)
        source_index_map: Dict mapping HDF5 file path -> (start_index, end_index) in imgList
        size_map:         Dict mapping display name -> "WxH" string (empty string if unknown)
    """
    sig = _dir_signature(dir_path)
    if sig is not None:
        # check in-memory cache first
        if dir_path in _SCAN_CACHE and _SCAN_CACHE[dir_path][0] == sig:
            cached = _SCAN_CACHE[dir_path][1]
            # Support old 3-tuple caches transparently
            if len(cached) == 3:
                return cached + ({},)
            return cached
        # try disk cache
        disk_payload = _load_scan_cache_from_disk(dir_path, sig, fallback_dir=fallback_cache_dir)
        if disk_payload is not None:
            _SCAN_CACHE[dir_path] = (sig, disk_payload)
            if len(disk_payload) == 3:
                return disk_payload + ({},)
            return disk_payload

    entries = []
    h5_files = []
    # Maps display_name -> "WxH" accumulated during scan
    size_map = {}

    try:
        file_names = os.listdir(dir_path)
    except Exception:
        return [], [], {}, {}

    for f in file_names:
        full_file_name = fullPath(dir_path, f)
        base, ext = os.path.splitext(f)
        if f == "calibration.tif":
            continue
        if ext.lower() in ('.hdf5', '.h5'):
            h5_files.append((base, ext, full_file_name))
        elif isImg(full_file_name) and ext.lower() not in ('.hdf5', '.h5'):
            # Add all TIFF files (no filtering here)
            entries.append((f, ("tiff", full_file_name)))
            # Read header-only size (very cheap, no pixel data decoded)
            shape = _tiff_size(full_file_name)
            if shape is not None:
                size_map[f] = f"{shape[1]}×{shape[0]}"

    # Filter out data HDF5 files if a corresponding master exists
    if h5_files:
        master_prefix_to_record = {}
        for base, ext, path in h5_files:
            if base.endswith('_master'):
                prefix = base[:-7]
                master_prefix_to_record[prefix] = (base, ext, path)

        filtered_h5 = []
        for base, ext, path in h5_files:
            if '_data_' in base:
                prefix = base.split('_data_')[0]
                if prefix in master_prefix_to_record:
                    # Skip data file because master exists
                    continue
            filtered_h5.append((base, ext, path))
        h5_files = filtered_h5

    # Count HDF5 frames (and capture shape) in parallel
    h5_positions = {}  # Will map path -> (start, end) before sorting
    # Maps h5 path -> shape tuple captured from _h5_nframes
    h5_shapes = {}
    if h5_files:
        if max_workers is None:
            try:
                max_workers = max(2, min(8, os.cpu_count() or 2))
            except Exception:
                max_workers = 2

        # Track progress if progress_dict provided
        if progress_dict is not None:
            progress_dict['h5_total'] = len(h5_files)
            progress_dict['h5_done'] = 0

        with ProcessPoolExecutor(max_workers=max_workers) as pool:
            paths = [p for _, _, p in h5_files]
            futures = [pool.submit(_h5_nframes, path) for path in paths]
            nframes_shape_list = []
            for i, future in enumerate(futures):
                nframes_shape_list.append(future.result())
                if progress_dict is not None:
                    progress_dict['h5_done'] = i + 1

        for (base, ext, path), (nframes, shape) in zip(h5_files, nframes_shape_list):
            if nframes <= 0:
                if progress_dict is not None:
                    progress_dict.setdefault('skipped_files', []).append(path)
                continue
            if shape is not None:
                h5_shapes[path] = shape
            start_idx = len(entries)
            if nframes == 1:
                disp = f"{base}_00001{ext}"
                entries.append((disp, ("h5", path, 0)))
                if shape is not None:
                    size_map[disp] = f"{shape[1]}×{shape[0]}"
            else:
                for i in range(nframes):
                    disp = f"{base}_{i+1:05d}{ext}"
                    entries.append((disp, ("h5", path, i)))
                    if shape is not None:
                        size_map[disp] = f"{shape[1]}×{shape[0]}"
            end_idx = len(entries) - 1
            h5_positions[path] = (start_idx, end_idx)

    entries.sort(key=lambda x: x[0])
    imgList = [n for n, _ in entries]
    specs = [s for _, s in entries]

    # Build final source_index_map after sorting
    source_index_map = {}
    for path in h5_positions.keys():
        indices = [i for i, spec in enumerate(specs)
                   if isinstance(spec, tuple) and len(spec) >= 2 and spec[1] == path]
        if indices:
            source_index_map[path] = (min(indices), max(indices))

    if sig is not None:
        payload = (imgList, specs, source_index_map, size_map)
        _SCAN_CACHE[dir_path] = (sig, payload)
        _save_scan_cache_to_disk(dir_path, sig, payload, fallback_dir=fallback_cache_dir)

    return imgList, specs, source_index_map, size_map
# --------------------- Unified image loader for GUI specs ---------------------
def load_image_via_spec(file_path, display_name, source):
    """
    Load an image array given a loader spec entry from GUI `fileList[1]`.
    - source can be ndarray, ("tiff", abs_path), or ("h5", abs_path, frame_idx)
    Applies ifHdfReadConvertless using display_name to handle HDF sentinel values.
    """
    if isinstance(source, np.ndarray):
        img = source
    elif isinstance(source, tuple):
        kind = source[0]
        if kind == "tiff" and len(source) == 2:
            img = fabio.open(source[1]).data
        elif kind == "h5" and len(source) == 3:
            abs_path, frame_idx = source[1], int(source[2])
            f = fabio.open(abs_path)
            try:
                if getattr(f, 'nframes', 1) == 1 or frame_idx == 0:
                    img = f.data if frame_idx == 0 else f.get_frame(frame_idx).data
                else:
                    img = f.get_frame(frame_idx).data
            finally:
                try:
                    f.close()
                except Exception:
                    pass
        else:
            img = fabio.open(fullPath(file_path, display_name)).data
    else:
        img = fabio.open(fullPath(file_path, display_name)).data

    img = ifHdfReadConvertless(display_name, img)
    return img

def get_loader_source(fileList, idx):
    try:
        return fileList[1][idx]
    except Exception:
        return None

def load_image_by_index(file_path, fileList, idx, display_name):
    source = get_loader_source(fileList, idx)
    return load_image_via_spec(file_path, display_name, source)

# --------------------- Reusable helpers for GUI provisional selection ---------------------
def build_provisional_selection(fullname):
    """
    Build a provisional selection from a single chosen file for immediate GUI display.
    Returns without redundant fields and without scanning the whole directory:
      (dir_path, imgList, current_index, loader_specs)

    - For HDF5, creates a pseudo-display name like base_00001.ext and a loader spec ("h5", path, 0)
    - For TIFF and other supported images, uses the filename and a loader spec ("tiff", path)
    """
    dir_path, sel_name = split(str(fullname))
    dir_path = str(dir_path)
    sel_name = str(sel_name)
    base, ext = os.path.splitext(sel_name)

    if ext.lower() in ('.h5', '.hdf5'):
        # If selected a data file and a matching master exists, pivot to master
        if "_data_" in base:
            prefix = base.split("_data_")[0]
            master_name = f"{prefix}_master{ext}"
            master_path = os.path.join(dir_path, master_name)
            if os.path.exists(master_path):
                sel_name = master_name
                base = f"{prefix}_master"
        disp = f"{base}_00001{ext}"
        imgList = [disp]
        loader_specs = [("h5", os.path.join(dir_path, sel_name), 0)]
    else:
        imgList = [sel_name]
        loader_specs = [("tiff", os.path.join(dir_path, sel_name))]

    current = 0
    return dir_path, imgList, current, loader_specs

class FileManager:
    """
    Two-layer navigation manager:
    1. File layer: for fast navigation and loading
    2. Image layer: for displaying total count and position (filled asynchronously in background)
    """
    def __init__(self):
        self.dir_path = ''
        self.output_dir = ''  # writable directory for scan cache fallback
        self.failedcases = None  # List of filenames to filter (from failedcases.txt)
        # File layer (fast, for navigation)
        self.file_list = []  # [(filename, type, full_path), ...]
        self.current_file_idx = 0
        self.current_frame_idx = 0  # Frame index within current file
        # Image layer (complete, for display)
        self.names = []  # Expanded display names
        self.specs = []  # Corresponding loader specs
        self.current = 0  # Current position in names
        # Currently loaded image ndarray (or None if not loaded)
        self.current_image = None
        self.current_file_type = None
        self.current_h5_nframes = None
        # HDF5 cache
        self._h5_frames = {}  # {full_path: nframes}
        self.source_index_map = {}  # {h5_path_or_dir_path: (start_idx, end_idx)} in names/specs
        self._path_to_file_idx = {}  # {full_path: file_idx} for fast reverse lookup
        # Per-image experiment label, parallel to names/specs
        self.source_labels: list = []  # source_labels[i] = experiment label for names[i]
        # Image sizes captured during scan: display_name -> "WxH"
        self.image_sizes: dict = {}
        # Async scan state
        self._scan_thread = None
        self._scan_done = False
        self._h5_progress = {'h5_total': 0, 'h5_done': 0}  # Track H5 processing
        self._scan_errors = []  # HDF5 files skipped due to load errors during background scan

    def set_from_file(self, selected_file):
        """
        Initialize from a selected file. Scans directory and locates the file.
        selected_file: full path of the file to select
        Supports: image files, HDF5 files, and failedcases.txt
        """
        # Extract directory path
        dir_path = os.path.dirname(str(selected_file))
        self.dir_path = dir_path
        
        # Check if selected file is failedcases.txt (must match exact name)
        selected_name = os.path.basename(str(selected_file))
        base, ext = os.path.splitext(selected_name)
        
        if ext.lower() == '.txt' and selected_name.lower() == 'failedcases.txt':
            # Read failedcases.txt
            self.failedcases = []
            try:
                with open(selected_file, 'r') as f:
                    for line in f:
                        filename = line.strip()
                        if filename:  # Ignore empty lines
                            self.failedcases.append(filename)
                print(f"Loaded {len(self.failedcases)} failed cases from {selected_name}")
            except Exception as e:
                print(f"Warning: Failed to read {selected_file}: {e}")
                self.failedcases = None
        else:
            self.failedcases = None
        
        # Scan directory for file list (fast, no HDF5 opening)
        # Note: failedcases filtering happens at image/frame level, not file level
        file_list = scan_directory_files_sync(dir_path)
        if not file_list:
            # Fallback to single file if scan fails (but not for .txt files)
            if ext.lower() != '.txt':
                fname = os.path.basename(str(selected_file))
                ftype = "h5" if ext.lower() in ('.h5', '.hdf5') else "tiff"
                file_list = [(fname, ftype, str(selected_file))]
        
        self.file_list = file_list
        self._rebuild_path_to_file_idx()
        
        # Locate selected file in the list
        self.current_file_idx = 0
        self.current_frame_idx = 0
        found = False
        
        # If .txt file was selected, use the first file in the filtered list
        if ext.lower() == '.txt':
            # No need to search, just use first file
            found = True if file_list else False
        # If selected a data file, try to find corresponding master first
        elif ext.lower() in ('.h5', '.hdf5') and "_data_" in base:
            prefix = base.split("_data_")[0]
            master_name = f"{prefix}_master{ext}"
            for i, (fname, ftype, fpath) in enumerate(file_list):
                if fname == master_name:
                    self.current_file_idx = i
                    found = True
                    break
        
        # If master not found or not a data file, find exact match
        if not found:
            for i, (fname, ftype, fpath) in enumerate(file_list):
                if fname == selected_name or fpath == str(selected_file):
                    self.current_file_idx = i
                    found = True
                    break
        
        # Build simple image layer (temporary, each HDF5 shown as single frame)
        self._rebuild_simple_image_list()
        # Ensure current image is loaded for the selected file
        self.load_current()

    def _rebuild_path_to_file_idx(self):
        """Rebuild path-to-file-index mapping for fast lookups"""
        self._path_to_file_idx = {fpath: i for i, (_, _, fpath) in enumerate(self.file_list)}
    
    def _rebuild_simple_image_list(self):
        """
        Rebuild simple image list (HDF5 shown as single frame).
        Applies failedcases filtering for TIFF files (filename == display name).
        HDF5 files are not filtered here (need frame expansion first).
        """
        names = []
        specs = []
        
        for fname, ftype, fpath in self.file_list:
            if ftype == "h5":
                # HDF5: show first frame as preview (filtering happens in full scan)
                base, ext = os.path.splitext(fname)
                disp = f"{base}_00001{ext}"
                # Check if this first frame is in failedcases
                if self.failedcases is None or disp in self.failedcases:
                    names.append(disp)
                    specs.append(("h5", fpath, 0))
            else:
                # TIFF: filename is display name, apply filter directly
                if self.failedcases is None or fname in self.failedcases:
                    names.append(fname)
                    specs.append(("tiff", fpath))
        
        # Maintain current position
        self.names = names
        self.specs = specs
        self.current = self.current_file_idx if self.current_file_idx < len(names) else 0

    def set_directory_listing(self, dir_path, names, specs, source_index_map=None, preserve_current_name=True, size_map=None, h5_index_map=None):
        """
        Set complete image list (from async scan, HDF5 expanded).
        Preserves currently selected file and frame.

        Args:
            source_index_map: Dict mapping H5 file path or dir path -> (start, end) in names.
            h5_index_map: Deprecated alias for source_index_map (accepted for backward compat).
        """
        if not names or not specs:
            return

        # backward-compat: honour old keyword if new one not supplied
        if source_index_map is None and h5_index_map is not None:
            source_index_map = h5_index_map
        
        # Remember current file and frame
        prev_file_path = None
        prev_frame = self.current_frame_idx
        if self.file_list and self.current_file_idx < len(self.file_list):
            prev_file_path = self.file_list[self.current_file_idx][2]
        
        self.dir_path = dir_path
        self.names = names
        self.specs = specs
        self.source_index_map = source_index_map if source_index_map is not None else {}
        if size_map:
            self.image_sizes.update(size_map)
        
        # Locate current image
        if prev_file_path:
            for i, spec in enumerate(specs):
                if isinstance(spec, tuple) and len(spec) >= 2:
                    spec_path = spec[1]
                    if spec_path == prev_file_path:
                        if len(spec) >= 3 and spec[2] == prev_frame:
                            self.current = i
                            break
                        elif i == len(specs) - 1 or (i + 1 < len(specs) and specs[i + 1][1] != prev_file_path):
                            # Found last or only frame of this file
                            self.current = i
                            break

    def start_async_scan(self, dir_path=None):
        """
        Start a background scan of the current directory (or provided dir_path) and
        update this FileManager's names/specs when finished. Safe for non-GUI thread.
        Returns the Thread object.
        """
        import threading

        if dir_path is not None:
            self.dir_path = dir_path

        self._scan_done = False
        self._h5_progress = {'h5_total': 0, 'h5_done': 0}  # Reset progress
        self._scan_errors = []  # Reset skipped-file list

        def _worker():
            fallback = self.output_dir if self.output_dir and self.output_dir != self.dir_path else None
            imgList, specs, source_index_map, size_map = scan_directory_images_cached(
                self.dir_path,
                progress_dict=self._h5_progress,
                fallback_cache_dir=fallback,
            )
            self._scan_errors = self._h5_progress.get('skipped_files', [])
            
            # Apply failedcases filtering if needed
            if self.failedcases is not None:
                filtered_names = []
                filtered_specs = []
                for name, spec in zip(imgList, specs):
                    if name in self.failedcases:
                        filtered_names.append(name)
                        filtered_specs.append(spec)
                imgList = filtered_names
                specs = filtered_specs
                # Note: source_index_map stays unchanged (refers to original complete list)
            
            try:
                # Update internal listing preserving current selection when possible
                self.set_directory_listing(self.dir_path, imgList, specs,
                                           source_index_map=source_index_map,
                                           preserve_current_name=True, size_map=size_map)
            finally:
                # Signal completion regardless of success
                self._scan_done = True

        self._scan_thread = threading.Thread(target=_worker, daemon=True)
        self._scan_thread.start()
        return self._scan_thread

    def is_scan_done(self):
        try:
            return bool(self._scan_done)
        except Exception:
            return False
    
    def get_h5_progress(self):
        """Get HDF5 processing progress (done, total)"""
        return self._h5_progress.get('h5_done', 0), self._h5_progress.get('h5_total', 0)
        

    def _get_current_file_info(self):
        """Get current file information"""
        if not self.file_list or self.current_file_idx >= len(self.file_list):
            return None, None, None
        
        fname, ftype, fpath = self.file_list[self.current_file_idx]
        return fname, ftype, fpath

    def _get_h5_nframes(self, path):
        """Get HDF5 file frame count (with caching). Returns int, not tuple."""
        if path not in self._h5_frames:
            nframes, _shape = _h5_nframes(path)
            self._h5_frames[path] = nframes
        return self._h5_frames[path]
    
    def get_current_h5_range(self):
        """Get current HDF5 file range"""
        fname, ftype, fpath = self._get_current_file_info()
        if fname is None:
            return None, None
        result = self.source_index_map.get(fpath)
        if result is None:
            return None, None
        return result

    def load_current(self):
        """Load current image"""
        fname, ftype, fpath = self._get_current_file_info()
        if fname is None:
            self.current_image = None
            self.current_image_name = ""
            self.current_h5_nframes = None
            return None
        
        if ftype == "h5":
            source = ("h5", fpath, self.current_frame_idx)
            self.current_h5_nframes = self._get_h5_nframes(fpath)
            # Prefer names[current] so that dir-sourced frames get prefix (e.g. "exp1/base_00001.h5"),
            # keeping the key consistent with what batch processing uses via names[i].
            if self.names and 0 <= self.current < len(self.names):
                self.current_image_name = self.names[self.current]
            else:
                base, ext = os.path.splitext(fname)
                self.current_image_name = f"{base}_{self.current_frame_idx+1:05d}{ext}"
        else:
            source = ("tiff", fpath)
            self.current_h5_nframes = None
            # Prefer names[current] so that dir-sourced images get prefix (e.g. "exp1/frame.tif"),
            # keeping the key consistent with what batch processing uses via names[i].
            if self.names and 0 <= self.current < len(self.names):
                self.current_image_name = self.names[self.current]
            else:
                self.current_image_name = fname
        img = load_image_via_spec(self.dir_path, fname, source)
        self.current_image = img
        self.current_file_type = ftype
        return img
    
    def get_image_by_index(self, index):
        """Return image array at the given image-layer index without changing selection.
        Returns None if index is out of range or loading fails.
        """
        try:
            if not self.names or not self.specs:
                return None
            idx = int(index)
            if idx < 0 or idx >= len(self.specs):
                return None
            display_name = self.names[idx]
            # Compose fileList tuple expected by loader helpers: (names, specs)
            fileList = (self.names, self.specs)
            return load_image_by_index(self.dir_path, fileList, idx, display_name)
        except Exception:
            return None
            
    def next_frame(self):
        """
        Next frame:
        - If current is HDF5 and has next frame, move to next frame
        - Otherwise move to first frame of next file
        """
        if not self.file_list:
            return
        
        fname, ftype, fpath = self._get_current_file_info()
        
        if ftype == "h5":
            nframes = self._get_h5_nframes(fpath)
            if self.current_frame_idx + 1 < nframes:
                # Move to next frame in same file
                self.current_frame_idx += 1
                self._update_current_position()
                self.load_current()
                return
        
        # Move to first frame of next file
        self.current_file_idx = (self.current_file_idx + 1) % len(self.file_list)
        self.current_frame_idx = 0
        self._update_current_position()
        self.load_current()

    def prev_frame(self):
        """
        Previous frame:
        - If current is HDF5 and not first frame, move to previous frame
        - Otherwise move to last frame of previous file
        """
        if not self.file_list:
            return
        
        if self.current_frame_idx > 0:
            # Move to previous frame in same file
            self.current_frame_idx -= 1
            self._update_current_position()
            self.load_current()
            return
        
        # Move to previous file
        self.current_file_idx = (self.current_file_idx - 1) % len(self.file_list)
        
        # Position to last frame of that file
        fname, ftype, fpath = self._get_current_file_info()
        if ftype == "h5":
            nframes = self._get_h5_nframes(fpath)
            self.current_frame_idx = max(0, nframes - 1)
        else:
            self.current_frame_idx = 0
        
        self._update_current_position()
        self.load_current()

    def _update_current_position(self):
        """Update current pointer to corresponding position in image layer"""
        fname, ftype, fpath = self._get_current_file_info()
        if fname is None:
            return
        
        # Find corresponding position in names/specs
        for i, spec in enumerate(self.specs):
            if isinstance(spec, tuple) and len(spec) >= 2:
                if spec[1] == fpath:
                    if ftype == "h5" and len(spec) >= 3:
                        if spec[2] == self.current_frame_idx:
                            self.current = i
                            return
                    else:
                        self.current = i
                        return
        
        # If not found (image layer not fully loaded yet), estimate position
        self.current = self.current_file_idx

    def get_display_name(self):
        """Get display name for current image"""
        fname, ftype, fpath = self._get_current_file_info()
        if fname is None:
            return ""
        
        if ftype == "h5":
            base, ext = os.path.splitext(fname)
            return f"{base}_{self.current_frame_idx+1:05d}{ext}"
        else:
            return fname

    def next_file(self):
        """Jump to first frame of next file"""
        if not self.file_list:
            return
        self.current_file_idx = (self.current_file_idx + 1) % len(self.file_list)
        self.current_frame_idx = 0
        self._update_current_position()
        self.load_current()

    def prev_file(self):
        """Jump to first frame of previous file"""
        if not self.file_list:
            return
        self.current_file_idx = (self.current_file_idx - 1) % len(self.file_list)
        self.current_frame_idx = 0
        self._update_current_position()
        self.load_current()
    
    def convert_frame_idx_to_image_idx(self, frame_idx):
        """
        Convert a frame index in the current HDF5 file to an image-layer index.
        Uses source_index_map for fast lookup when available.
        Returns an integer index in `self.names/specs`, or None if not resolvable.
        """
        try:
            fname, ftype, fpath = self._get_current_file_info()
            if not fname or ftype != "h5" or not self.specs:
                return None

            # Normalize frame index within known range if available
            if isinstance(self.current_h5_nframes, int) and self.current_h5_nframes > 0:
                if frame_idx < 0:
                    frame_idx = 0
                elif frame_idx >= self.current_h5_nframes:
                    frame_idx = self.current_h5_nframes - 1

            # Fast path: use source_index_map if available
            if fpath in self.source_index_map:
                start_idx, end_idx = self.source_index_map[fpath]
                # The frames should be sequential within this range
                target_idx = start_idx + frame_idx
                if start_idx <= target_idx <= end_idx:
                    # Verify it's the correct frame
                    if (target_idx < len(self.specs) and 
                        isinstance(self.specs[target_idx], tuple) and 
                        len(self.specs[target_idx]) >= 3 and
                        self.specs[target_idx][1] == fpath and
                        self.specs[target_idx][2] == frame_idx):
                        return target_idx

            return None
        except Exception:
            return None
    
    def switch_image_by_index(self, index):
        """Switch to an image by its index"""
        if not self.names or index < 0 or index >= len(self.names):
            return
        self.current = index
        if index < len(self.specs):
            spec = self.specs[index]
            if isinstance(spec, tuple) and len(spec) >= 2:
                target_path = spec[1]
                
                # Fast lookup of file index using cache
                file_idx = self._path_to_file_idx.get(target_path)
                if file_idx is None:
                    # Cache miss - rebuild cache and retry
                    self._rebuild_path_to_file_idx()
                    file_idx = self._path_to_file_idx.get(target_path)
                
                if file_idx is not None:
                    self.current_file_idx = file_idx
                    # Frame index is already in spec[2] for H5, or 0 for TIFF
                    self.current_frame_idx = spec[2] if len(spec) >= 3 else 0
        self.load_current()

    def switch_image_by_name(self, name):
        """
        Switch to an image by its display name.
        Returns True if successful, False if name not found.
        """
        if not self.names:
            return False
        
        try:
            # Find the index of the image with the given name
            index = self.names.index(name)
            self.switch_image_by_index(index)
            return True
        except ValueError:
            return False

    def load_from_sources(self, sources: list):
        """Load images from a mixed list of H5 file paths and directory paths.

        Each source becomes one experiment entry in ``source_index_map``.

        Naming rules:
            H5 file source  → frames named ``base_00001.h5`` (no directory prefix).
                               ``source_labels[i]`` = basename of the H5 file without extension.
            Dir source      → images named ``dirname/image.tif`` or ``dirname/base_00001.h5``.
                               ``source_labels[i]`` = basename of the directory.

        Populates:
            self.source_index_map  {source_path: (start_idx, end_idx)} in names/specs
            self.source_labels     experiment label per position (parallel to names/specs)
            self.file_list         merged file layer (absolute paths)
            self.names / specs     merged image layer
            self.image_sizes       merged size map

        Does NOT start a background scan – all scanning is done synchronously
        so the caller immediately gets a consistent view.
        """
        # Stop any residual async scan so _scan_done guards won't misfire.
        self._scan_done = True
        self._scan_thread = None

        all_file_list = []
        all_names = []
        all_specs = []
        all_labels = []
        all_sizes = {}
        self.source_index_map = {}

        for source in sources:
            source = str(source)
            base_ext = os.path.splitext(os.path.basename(source))
            base, ext = base_ext

            if os.path.isfile(source) and ext.lower() in ('.h5', '.hdf5'):
                # --- H5 file as experiment (no prefix) ---
                label = base  # e.g. "exp1_master" without extension
                nframes, shape = _h5_nframes(source)
                if nframes <= 0:
                    continue
                all_file_list.append((os.path.basename(source), "h5", source))
                start_idx = len(all_names)
                for i in range(nframes):
                    disp = f"{base}_{i + 1:05d}{ext}"
                    all_names.append(disp)
                    all_specs.append(("h5", source, i))
                    all_labels.append(label)
                    if shape is not None:
                        all_sizes[disp] = f"{shape[1]}×{shape[0]}"
                end_idx = len(all_names) - 1
                if start_idx <= end_idx:
                    self.source_index_map[source] = (start_idx, end_idx)

            elif os.path.isdir(source):
                # --- Directory as experiment (prefixed names) ---
                prefix = os.path.basename(source.rstrip('/\\')) or source
                label = prefix

                # File layer (fast, no HDF5 opening)
                for entry in scan_directory_files_sync(source):
                    all_file_list.append(entry)

                # Image layer (full scan; uses disk cache when available)
                names, specs, _h5_map, size_map = scan_directory_images_cached(source)

                start_idx = len(all_names)
                for name, spec in zip(names, specs):
                    all_names.append(f"{prefix}/{name}")
                    all_specs.append(spec)
                    all_labels.append(label)
                end_idx = len(all_names) - 1

                if start_idx <= end_idx:
                    self.source_index_map[source] = (start_idx, end_idx)

                all_sizes.update({f"{prefix}/{k}": v for k, v in size_map.items()})

        self.file_list = all_file_list
        self._rebuild_path_to_file_idx()
        self.names = all_names
        self.specs = all_specs
        self.source_labels = all_labels
        self.image_sizes = all_sizes
        # Set dir_path to the common parent directory of all sources, not the first source
        # itself (which may be a subdirectory or H5 file). The UI guarantees all sources
        # share the same parent, so dirname of any source (after stripping trailing sep) is enough.
        if sources:
            self.dir_path = os.path.dirname(str(sources[0]).rstrip('/\\'))
        else:
            self.dir_path = ""
        self.current = 0
        self.current_file_idx = 0
        self.current_frame_idx = 0

        if all_names:
            self.load_current()

    def load_from_directories(self, dir_paths: list):
        """Backward-compatible shim — delegates to load_from_sources."""
        self.load_from_sources(dir_paths)


def scan_directory_files_sync(dir_path):
    """
    Synchronously scan directory for files without opening HDF5 to count frames.
    
    Args:
        dir_path: Directory path to scan
    
    Returns: 
        file_list [(filename, type, full_path), ...]
    
    This is a fast scan that only lists files, does not open any HDF5 files.
    Note: Does not apply failedcases filtering - that happens at the image/frame level.
    """
    files = []
    try:
        file_names = os.listdir(dir_path)
    except Exception:
        return []

    for f in file_names:
        full_path = fullPath(dir_path, f)
        base, ext = os.path.splitext(f)
        
        if f == "calibration.tif":
            continue
            
        if ext.lower() in ('.h5', '.hdf5'):
            # Check if this is a data file, skip if corresponding master exists
            if '_data_' in base:
                prefix = base.split('_data_')[0]
                master_name = f"{prefix}_master{ext}"
                master_path = os.path.join(dir_path, master_name)
                if os.path.exists(master_path):
                    continue  # Skip data file
            files.append((f, "h5", full_path))
        elif isImg(full_path):
            files.append((f, "tiff", full_path))
    
    files.sort(key=lambda x: x[0])
    return files