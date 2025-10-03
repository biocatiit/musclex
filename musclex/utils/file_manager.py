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

def getBlankImageAndMask(path):
    """
    Give the blank image and the mask threshold saved in settings
    :return: blankImage, mask threshold
    """
    mask_file = join(join(path, 'settings'),'mask.tif')
    blank_file = join(join(path, 'settings'),'blank.tif')
    mask = None
    blank_img = None
    if exists(mask_file):
        mask = fabio.open(mask_file).data
    if exists(blank_file):
        blank_img = fabio.open(blank_file).data
    return blank_img, mask

def getMaskOnly(path):
    """
    Give only the mask threshold
    :param path: file path
    :return: mask threshold
    """
    maskonly_file = join(join(path, 'settings'),'maskonly.tif')
    if exists(maskonly_file):
        return fabio.open(maskonly_file).data
    return None

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
        return join(cdir, "scan_cache.pkl")
    except Exception:
        return None

def _load_scan_cache_from_disk(dir_path, sig):
    try:
        cfile = _disk_cache_file(dir_path)
        if cfile and exists(cfile):
            with open(cfile, "rb") as f:
                data = pickle.load(f)
            if isinstance(data, dict) and data.get("sig") == sig and isinstance(data.get("payload"), tuple):
                return data.get("payload")
    except Exception:
        pass
    return None

def _save_scan_cache_to_disk(dir_path, sig, payload):
    try:
        cdir = _disk_cache_dir(dir_path)
        if cdir and not exists(cdir):
            os.makedirs(cdir, exist_ok=True)
        cfile = _disk_cache_file(dir_path)
        if cfile:
            with open(cfile, "wb") as f:
                pickle.dump({"sig": sig, "payload": payload}, f)
    except Exception:
        # best-effort; ignore failures
        pass

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

def _h5_nframes(path):
    try:
        f = fabio.open(path)
        n = getattr(f, 'nframes', 1)
        try:
            f.close()
        except Exception:
            pass
        return n
    except Exception:
        return 0

def scan_directory_images_cached(dir_path, failedcases=None, max_workers=None):
    """
    Scan a directory for TIFF and HDF5 images and return unified (imgList, loader_specs).
    Uses a cache keyed by directory content signature. HDF5 frame counts are computed
    in parallel using processes. Frames are NOT loaded.
    """
    sig = _dir_signature(dir_path)
    if sig is not None:
        # check in-memory cache first
        if dir_path in _SCAN_CACHE and _SCAN_CACHE[dir_path][0] == sig:
            return _SCAN_CACHE[dir_path][1]
        # try disk cache
        disk_payload = _load_scan_cache_from_disk(dir_path, sig)
        if disk_payload is not None:
            _SCAN_CACHE[dir_path] = (sig, disk_payload)
            return disk_payload

    entries = []
    h5_files = []

    try:
        file_names = os.listdir(dir_path)
    except Exception:
        return [], []

    for f in file_names:
        if failedcases is not None and f not in failedcases:
            continue
        full_file_name = fullPath(dir_path, f)
        base, ext = os.path.splitext(f)
        if f == "calibration.tif":
            continue
        if ext.lower() in ('.hdf5', '.h5'):
            h5_files.append((base, ext, full_file_name))
        elif isImg(full_file_name) and ext.lower() not in ('.hdf5', '.h5'):
            entries.append((f, ("tiff", full_file_name)))

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

    # Count HDF5 frames in parallel
    if h5_files:
        if max_workers is None:
            try:
                max_workers = max(2, min(8, os.cpu_count() or 2))
            except Exception:
                max_workers = 2
        with ProcessPoolExecutor(max_workers=max_workers) as pool:
            paths = [p for _, _, p in h5_files]
            nframes_list = list(pool.map(_h5_nframes, paths))
        for (base, ext, path), nframes in zip(h5_files, nframes_list):
            if nframes <= 0:
                continue
            if nframes == 1:
                disp = f"{base}_00001{ext}"
                entries.append((disp, ("h5", path, 0)))
            else:
                for i in range(nframes):
                    disp = f"{base}_{i+1:05d}{ext}"
                    entries.append((disp, ("h5", path, i)))

    entries.sort(key=lambda x: x[0])
    imgList = [n for n, _ in entries]
    specs = [s for _, s in entries]

    if sig is not None:
        payload = (imgList, specs)
        _SCAN_CACHE[dir_path] = (sig, payload)
        _save_scan_cache_to_disk(dir_path, sig, payload)

    return imgList, specs
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
    Minimal stateful manager for image navigation/loading.
    Used by GUIs to keep a single source of truth for names/specs/current.
    """
    def __init__(self):
        self.dir_path = ''
        self.names = []
        self.specs = []
        self.current = 0

    def set_provisional(self, fullname):
        dir_path, names, current, specs = build_provisional_selection(fullname)
        self.dir_path = dir_path
        self.names = names
        self.specs = specs
        self.current = current

    def set_directory_listing(self, dir_path, names, specs, preserve_current_name=True):
        prev_name = self.names[self.current] if preserve_current_name and self.names else None
        self.dir_path = dir_path
        self.names = names or []
        self.specs = specs or []
        if prev_name and prev_name in self.names:
            self.current = self.names.index(prev_name)
        else:
            self.current = 0

    def load_current(self):
        if not self.names or not self.specs:
            return None
        name = self.names[self.current]
        source = None
        try:
            source = self.specs[self.current]
        except Exception:
            source = None
        return load_image_via_spec(self.dir_path, name, source)

    def next_frame(self):
        if not self.names:
            return
        self.current = (self.current + 1) % len(self.names)

    def prev_frame(self):
        if not self.names:
            return
        self.current = (self.current - 1) % len(self.names)

    def _basename(self, idx):
        try:
            spec = self.specs[idx]
            path = spec[1] if isinstance(spec, tuple) and len(spec) >= 2 else ''
            return os.path.basename(path)
        except Exception:
            return ''

    def next_file(self):
        if not self.names:
            return
        n = len(self.names)
        cur_name = self._basename(self.current)
        i = (self.current + 1) % n
        while i != self.current:
            if self._basename(i) != cur_name:
                self.current = i
                return
            i = (i + 1) % n

    def prev_file(self):
        if not self.names:
            return
        n = len(self.names)
        cur_name = self._basename(self.current)
        i = (self.current - 1 + n) % n
        while i != self.current:
            if self._basename(i) != cur_name:
                # jump to the first frame of this previous file
                base = self._basename(i)
                # walk back to find its first frame
                j = i
                while True:
                    k = (j - 1 + n) % n
                    if self._basename(k) != base or k == i:
                        break
                    j = k
                self.current = j
                return
            i = (i - 1 + n) % n

def async_scan_directory(dir_path, on_done):
    """
    Start a background scan of a directory using scan_directory_images_cached and invoke
    on_done(imgList, specs) when finished. Returns the Thread object.

    Note: on_done may be executed on a non-GUI thread; if using Qt, marshal back to the main
    thread (e.g., via signals/QTimer) before touching widgets.
    """
    import threading

    def _worker():
        imgList, specs = scan_directory_images_cached(dir_path)
        try:
            on_done(imgList, specs)
        except Exception:
            pass

    t = threading.Thread(target=_worker, daemon=True)
    t.start()
    return t
