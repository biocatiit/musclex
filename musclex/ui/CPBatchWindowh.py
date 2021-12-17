from .pyqt_utils import *
from matplotlib.patches import Ellipse, Rectangle, FancyArrow
from matplotlib.collections import PatchCollection
from matplotlib.colors import LogNorm, Normalize
from scipy.interpolate import Rbf
import h5py
import os
from ..utils.file_manager import *
from ..modules.ScanningDiffraction import *
from ..csv_manager import CP_CSVManager
import musclex
import pandas as pd
import numpy as np
from .CPImageWindowh import CPImageWindowh
import json


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
        except:
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

class CPBatchWindowh():
    def __init__(self, dir_path="",inputsetting=False,delcache=False,settingspath=None):
        self.filePath = dir_path
       
        self.inputsetting=inputsetting
        self.delcache=delcache
        self.settingspath=settingspath

        self.csvManager = CP_CSVManager(self.filePath)

        self.processFolder(self.filePath)


    def browseHDF(self, dir_path, hdfList=[]):
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
            # self.processBatchmodeResults()
       

    # def processBatchmodeResults(self):
    #     dir_path = self.filePath
    #     self.csvManager.load_all()
    #     hdf_filename = self.hdf_filename
    #     csv_filename = self.csvManager.sum_file
    #     self.updateStatusBar(text='Dir : ' + dir_path + '\nHDF : ' + hdf_filename + '\nCSV : ' + csv_filename)
    #     df_sum = self.csvManager.df_sum.copy()
    #     df_sum = df_sum.sort_values(['filename'], ascending=True)
    #     df_rings = self.csvManager.df_rings

    #     # Read intensity from csv to organize the info given
    #     self.name_dict = {}
    #     self.intensity_dict = {}
    #     self.sim_inten_dict = {}
    #     self.peak_intensity_dict = {}
    #     self.distance_dict = {}
    #     self.angrange_dict = {}
    #     self.orientation_dict = {}
    #     self.fit_dict = {}
    #     self.fitcd_dict = {}

    #     for i, row in df_sum.iterrows():
    #         filename = str(row['filename'])
    #         start_ind = filename.rfind('_')
    #         end_ind = filename.rfind('.')
    #         index = int(row['filename'][start_ind + 1:end_ind])
    #         self.name_dict[index] = row['filename']
    #         self.intensity_dict[index] = row['total intensity (hull)'] \
    #                 if 'total intensity (hull)' in row and not np.isnan(row['total intensity (hull)']) else \
    #                 row['total intensity']
    #         self.sim_inten_dict[index] = row['total intensity']
    #         #print(np.isnan(row['total intensity (hull)']), self.intensity_dict[index], self.sim_inten_dict[index])

    #         # Add ring model if its error < 1. and sigma < 1. (prevent uniform ring)
    #         all_rings = df_rings[df_rings['filename'] == filename]
    #         if len(all_rings) > 0:
    #             distance_ok = True
    #             if self.bestRadio.isChecked():
    #                 all_rings = all_rings.sort_values(['angle fitting error'], ascending=True)
    #                 best_ring = all_rings.iloc[0]
    #             else:
    #                 dist = self.distanceSpnBx.value()
    #                 unit = str(self.unitChoice.currentText())
    #                 if unit == 'pixel':
    #                     col = 'S'
    #                 else:
    #                     col = 'd'
    #                 try:
    #                     min_ind = min(np.arange(len(all_rings)), key=lambda ind: abs(float(all_rings.iloc[ind][col])-dist)) # Find closest ring to distance
    #                     max_dif = self.bandwidthSpnBx.value()
    #                     if abs(float(all_rings.iloc[min_ind][col])-dist) > max_dif:
    #                         distance_ok = False
    #                 except:
    #                     print("WARNING : Unable to find the closest ring to the specified d-spacing for image %s" % (row['filename']))
    #                     min_ind = 0
    #                     distance_ok = False
    #                 best_ring = all_rings.iloc[min_ind]

    #             good_model = float(best_ring['angle fitting error']) < 1. and best_ring['angle sigma'] < 1. and distance_ok
    #             peak_inten = -1
    #             d_spacing = 0
    #             angle = 0
    #             angle_sigma = 0

    #             if good_model:
    #                 peak_inten = float(best_ring['peak intensity']) if pd.notnull(best_ring['peak intensity']) else 0
    #                 angle = best_ring['angle'] if pd.notnull(best_ring['angle']) else 0
    #                 angle_sigma = float(best_ring['angle sigma']) if pd.notnull(best_ring['angle sigma']) else 0
    #                 if pd.notnull(best_ring['d']) and best_ring['d'] != '-':
    #                     d_spacing = float(best_ring['d'])
    #                 elif pd.notnull(best_ring['S']):
    #                     d_spacing = float(best_ring['S'])


    #             self.peak_intensity_dict[index] = peak_inten
    #             self.orientation_dict[index] = angle
    #             self.angrange_dict[index] = angle_sigma
    #             self.distance_dict[index] = d_spacing
    #         else:
    #             self.peak_intensity_dict[index] = -1
    #             self.orientation_dict[index] = 0
    #             self.angrange_dict[index] = 0
    #             self.distance_dict[index] = 0

    #     self.init_number = min(self.name_dict.keys())

    #     # Read hdf5 file to get the coordinates and image shape
    #     self.hdf_data = self.get_scan_data(hdf_filename)
    #     self.coord_dict = {}

    #     for i in range(self.init_number, len(self.hdf_data) + self.init_number):
    #         self.coord_dict[i] = (self.hdf_data[i - self.init_number][0], self.hdf_data[i - self.init_number][1])
    #     nCols = len(self.hdf_data) # 1D Scan
    #     for i in range(self.init_number + 1, len(self.hdf_data) + self.init_number):
    #         if abs(self.coord_dict[i][1] - self.coord_dict[i - 1][1]) != 0:
    #             nCols = i - self.init_number
    #             break
    #     if nCols != 0:
    #         nRows = int(len(self.hdf_data) / nCols)
    #     else :
    #         nRows = 0
    #     all_xs = np.reshape(np.array([v[0] for k, v in self.coord_dict.items()]), (nRows, nCols))
    #     all_ys = np.reshape(np.array([v[1] for k, v in self.coord_dict.items()]), (nRows, nCols))
    #     x = np.mean(all_xs, axis=0)
    #     y = np.mean(all_ys, axis=1)

    #     if len(x) > 1:
    #         x_grad = abs(x[1] - x[0])
    #     else:
    #         x_grad = 0
    #     if len(y) > 1:
    #         y_grad = abs(y[1] - y[0])
    #     else:
    #         y_grad = 0

    #     # Plot heatmap for intensity
    #     z = [float(self.intensity_dict[i]) if i in self.intensity_dict else -1 for i in
    #          range(self.init_number, len(self.hdf_data) + self.init_number)]
    #     simp_z = [float(self.sim_inten_dict[i]) if i in self.sim_inten_dict else -1 for i in
    #          range(self.init_number, len(self.hdf_data) + self.init_number)]
    #     dist_z = [float(self.distance_dict[i]) if i in self.distance_dict else -1 for i in
    #          range(self.init_number, len(self.hdf_data) + self.init_number)]
    #     # z = np.array([z[i:i + x_max] for i in range(0, , x_max)])
    #     # z = cv2.blur(z, (4,4))
    #     # intensity = np.array(z)
    #     ring_z = [float(self.peak_intensity_dict[i]) if i in self.peak_intensity_dict else -1 for i in
    #          range(self.init_number, len(self.hdf_data) + self.init_number)]
    #     intensity = np.reshape(z, (len(y), len(x)))
    #     simp_intensity = np.reshape(simp_z, (len(y), len(x)))
    #     ring_intensity = np.reshape(ring_z, (len(y), len(x)))
    #     intensity = np.ma.array(intensity, mask=intensity < 0)
    #     simp_intensity = np.ma.array(simp_intensity, mask=simp_intensity < 0)
    #     ring_intensity = np.ma.array(ring_intensity, mask=ring_intensity < 0)
    #     dspace_intensity = np.reshape(dist_z, (len(y), len(x)))

    #     self.xyIntensity = [x, y, intensity, ring_intensity, simp_intensity, dspace_intensity]
    #     self.beamXSpinBox.setValue(x_grad)
    #     self.beamXSpinBox.setMaximum(x_grad * 2)
    #     self.beamYSpinBox.setValue(y_grad)
    #     self.beamYSpinBox.setMaximum(y_grad * 2)
    #     self.xylim = [self.beamXSpinBox.value(), self.beamYSpinBox.value()]
    #     self.refreshAllTabs()
    #     self.updateImage()
    #     self.updateUI()
    #     QApplication.restoreOverrideCursor()

    def convert_to_float(self, i):
        return float(i) if i.replace('.', '', 1).replace('-', '').isdigit() else i

    def get_scan_data(self, filename):
        if h5py.is_hdf5(filename):
            hf = h5py.File(filename, 'r')
            return np.array(hf.get('data').get('BL'))
        elif os.path.isdir(filename):
            return sorted(self.parse_logfiles_dir(filename), key=lambda x: (x[1], x[0]))
        elif filename.endswith('.log'):
            return self.parse_logfile(filename)

    def parse_logfile(self, filename):
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
        files = os.listdir(dir_name)
        data = []
        for f in files:
            if f.endswith('.log'):
                scans = self.parse_logfile(os.path.join(dir_name, f))
                data.extend(scans)
        return data



    def processFolder(self, dir_path):
        hdf_path=fullPath(dir_path,'settings')
        if os.path.exists(hdf_path):
            if os.path.exists(fullPath(hdf_path,'file.hdf')):
                os.remove(fullPath(hdf_path,'file.hdf'))
            if os.path.exists(fullPath(hdf_path,'hdf.info')):
                os.remove(fullPath(hdf_path,'hdf.info'))
        input_types = ['.adsc', '.cbf', '.edf', '.fit2d', '.mar345', '.marccd', '.pilatus', '.tif', '.hdf5', '.smv']

        if dir_path != "":
            imgList = os.listdir(dir_path)
            imgList.sort()
        for image in imgList:
            file_name=os.path.join(dir_path,image)
            if os.path.isfile(file_name):
                _, ext = os.path.splitext(str(file_name))
                if ext in input_types:
                    cp = CPImageWindowh(image, dir_path,self.inputsetting,self.delcache,self.settingspath)
        
        imgList, hdfList = getFilesAndHdf(dir_path)
        self.browseHDF(dir_path, hdfList)

 
   

def convertRadtoDegreesEllipse(rad):
    return rad * 180. / np.pi
