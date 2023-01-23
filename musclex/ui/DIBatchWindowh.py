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
import json
import h5py
import numpy as np
from .pyqt_utils import *
from ..utils.file_manager import *
from ..modules.ScanningDiffraction import *
from ..csv_manager import DI_CSVManager
from .DIImageWindowh import DIImageWindowh

matplotlib.rcParams.update({'font.size': 5})

class HDFBrowser():
    """
    Provide options for HDF Browser - select or create one
    """
    def __init__(self, msg, setting_path):
        """
        initial dialog
        :param msg: message which appear in dialog
        :param setting_path: path that HDF file will be saved
        """
        #super(self).__init__(None)
        self.msg = msg
        self.path = setting_path
        self.hdf_file = ""
        self.okClicked()

    def okClicked(self):
        """
        Handle when OK is clicked
        """
        # Generate HDF file
        self.hdf_file = join(self.path, 'file.hdf')
        filename=fullPath(self.path, 'dihdf.json')
        try:
            with open(filename) as f:
                print('hdf step size file provided at', filename)
                dihdf=json.load(f)
                print(dihdf)
                print('If you need to change the step size, go to the interactive version')
        except Exception:
            print('hdf step file not detected, step size use default settings:')
            print('x_start:0, x_end:100, x_step:1')
            print('y_start:0, y_end:100, y_step:1')
            print('To create different step size, use the interactive version, step size will be saved automatically')
            dihdf={"x_start": 0.0, "y_start": 0.0, "x_end": 100.0, "y_end": 100.0, "x_step": 1, "y_step": 1}
        x_start=dihdf['x_start']
        y_start=dihdf['y_start']
        x_end=dihdf['x_end']
        y_end=dihdf['y_end']
        x_step=dihdf['x_step']
        y_step=dihdf['y_step']

        # The total size divided by the step size is the number of steps in
        # both directions
        # Check if end - start = 0 for either - if it is then run in
        # 1D scan mode?
        if x_step != 0:
            x_nStep = int(np.round((x_end - x_start) / x_step) + 1)
        else:
            x_nStep = 1

        if y_step != 0:
            y_nStep = int(np.round((y_end - y_start) / y_step) + 1)
        else:
            y_nStep = 1

        data = []
        for j in range(0, y_nStep):
            y = y_start + j*y_step
            for i in range(0, x_nStep):
                x = x_start + i*x_step
                data.append((x, y))

        hf = h5py.File(self.hdf_file, 'w')
        data_grp = hf.create_group("data")
        data_grp.create_dataset("BL", data=data)
        hf.close()

class DIBatchWindowh():
    """
    A class to process Scanning diffraction on folders (headless)
    """
    def __init__(self, dir_path="",inputsetting=False,delcache=False,settingspath=None):
        self.filePath = dir_path
        self.inputsetting=inputsetting
        self.delcache=delcache
        self.settingspath=settingspath
        self.hdf_filename = ""

        self.csvManager = DI_CSVManager(self.filePath)

        self.processFolder(self.filePath)

    def browseHDF(self, dir_path, hdfList=[]):
        """
        Browse HDF files
        """
        hdf_filename = ""
        path = join(dir_path, 'settings')
        createFolder(path)
        hdf_cache = join(path, 'hdf.info')

        if exists(hdf_cache):
            hdf_filename = pickle.load(open(hdf_cache, "rb"))

        if len(hdf_filename) == 0 or not exists(hdf_filename):
            if len(hdfList) == 1:
                hdf_filename = join(dir_path, hdfList[0])
            else:
                if len(hdfList) == 0:
                    dlg = HDFBrowser('No HDF file detected.\nPlease select an HDF file to process or create a new one.', path)
                else:
                    dlg = HDFBrowser('There are more than one HDF file detected. \nPlease select an HDF file to process or create a new one.', path)
                hdf_filename = dlg.hdf_file

        if hdf_filename != "":
            pickle.dump(hdf_filename, open(hdf_cache, "wb"))
            self.hdf_filename = str(hdf_filename)

    def convert_to_float(self, i):
        """
        Convert i to float
        """
        return float(i) if i.replace('.', '', 1).replace('-', '').isdigit() else i

    def get_scan_data(self, filename):
        """
        Give the scanned data
        """
        if h5py.is_hdf5(filename):
            hf = h5py.File(filename, 'r')
            return np.array(hf.get('data').get('BL'))
        elif os.path.isdir(filename):
            return sorted(self.parse_logfiles_dir(filename), key=lambda x: (x[1], x[0]))
        elif filename.endswith('.log'):
            return self.parse_logfile(filename)
        return None

    def parse_logfile(self, filename):
        """
        Parse the log file
        """
        data_dir, fname = os.path.split(filename)
        count_filename = os.path.join(data_dir, fname)

        with open(count_filename, 'r') as f:
            all_lines = f.readlines()

        line_num = 0
        for i, line in enumerate(all_lines):
            if not line.startswith('#'):
                line_num = i
                break

        headers = all_lines[line_num - 1].replace('\n', '').split('\t')
        x_index = headers.index('x')
        y_index = headers.index('y')

        print(f'Log Headers: {headers},\n x index: {x_index},\n y index: {y_index}')
        scans = []
        for i in range(line_num, len(all_lines)):
            data = all_lines[i].replace('\n', '').split('\t')
            scans.append(list(map(self.convert_to_float, [data[x_index], data[y_index]])))
        return scans

    def parse_logfiles_dir(self, dir_name):
        """
        Parse the directory log file
        """
        files = os.listdir(dir_name)
        data = []
        for f in files:
            if f.endswith('.log'):
                scans = self.parse_logfile(os.path.join(dir_name, f))
                data.extend(scans)
        return data

    def processFolder(self, dir_path):
        """
        Process the folder selected
        """
        hdf_path=fullPath(dir_path,'settings')
        if os.path.exists(hdf_path):
            if os.path.exists(fullPath(hdf_path,'file.hdf')):
                os.remove(fullPath(hdf_path,'file.hdf'))
            if os.path.exists(fullPath(hdf_path,'hdf.info')):
                os.remove(fullPath(hdf_path,'hdf.info'))
        inpt_types = ['.adsc', '.cbf', '.edf', '.fit2d', '.mar345', '.marccd', '.pilatus', '.tif', '.hdf5', '.smv']

        if dir_path != "":
            imgList = os.listdir(dir_path)
            imgList.sort()
        from multiprocessing import Lock, Process, cpu_count
        lock = Lock()
        procs = []
        for image in imgList:
            file_name=os.path.join(dir_path,image)
            if os.path.isfile(file_name):
                _, ext = os.path.splitext(str(file_name))
                if ext in inpt_types:
                    # DIImageWindowh(image, dir_path,self.inputsetting,self.delcache,self.settingspath)
                    proc = Process(target=DIImageWindowh, args=(image, dir_path, self.inputsetting, self.delcache, self.settingspath, lock,))
                    procs.append(proc)
                    proc.start()
            if len(procs) % cpu_count() == 0:
                for proc in procs:
                    proc.join()
                procs = []
        for proc in procs:
            proc.join()

        imgList, hdfList = getFilesAndHdf(dir_path)
        self.browseHDF(dir_path, hdfList)

def convertRadtoDegreesEllipse(rad):
    """
    Convert radian to degrees
    """
    return rad * 180. / np.pi
