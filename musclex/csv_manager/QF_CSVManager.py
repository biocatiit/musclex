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
from os import makedirs
from os.path import exists
# import hashlib
import pandas as pd
from datetime import datetime
try:
    from ..utils.file_manager import fullPath
except: # for coverage
    from utils.file_manager import fullPath

class QF_CSVManager:
    """
    A class taking care of writing results including csv file and failedcases file
    """
    def __init__(self, dir_path, extra_colnames=None, version=None):
        """
        init with directory path
        :param dir_path:
        :param extra_colnames:
        :param version:
        """
        self.dataframe = None
        result_path = fullPath(dir_path, "qf_results")
        if not exists(result_path):
            makedirs(result_path)
        self.filename = fullPath(result_path, 'summary.csv')
        self.colnames = [
            'Filename', 'version', 'date', 'centerX', 'centerY', 'rotationAngle',
            'backgroundMethod', 'backgroundConfigName',
            'parameters', 'downsampled',
            'loss', 'bgSum', 'symmetry',#, 'hash', 'comment'
            # Blank/mask + calibration user settings. These live in the
            # SettingsManager (not qfsettings.json) and are read off the
            # ImageData attached to the QuadrantFolder at write time, so
            # GUI and headless report the same values.
            'blank_enabled', 'mask_enabled', 'blank_weight',
            'detector', 'lambda_sdd',
        ]
        self.version = version if version is not None else 'unknown'

        if extra_colnames:
            self.colnames.extend(extra_colnames)
        
        self.loadFailedCases(dir_path)
        self.loadSummary()

    def loadFailedCases(self, direc):
        """
        Load failed cases file from the directory and keep them in self.failedcases
        :param direc: input directory (str)
        :return: -
        """
        self.failedcasesfile = fullPath(direc, "failedcases.txt")
        self.failedcases = set()
        if exists(self.failedcasesfile):
            for line in open(self.failedcasesfile, 'r'):
                name = line.rstrip('\n')
                self.failedcases.add(name)

    def loadSummary(self):
        """
        Load summary.csv file and keep data in self.dataframe
        :return:
        """
        if not exists(self.filename):
            self.dataframe = pd.DataFrame(columns = self.colnames)
        else:
            self.dataframe = pd.read_csv(self.filename)
            for col in self.colnames:
                if col not in self.dataframe.columns:
                    self.dataframe[col] = '-'


    def writeNewData(self, quadFold):
        """
        Add new data to dataframe, then re-write summary.csv and failed cases file
        :param quadFold: QuadrantFolder object with results in its info dict
        :return: -
        """
        self.loadSummary()
        img_name = quadFold.img_name
        cache = quadFold.imgCache
        self.removeData(img_name)
        data = {}

        processed_flags = quadFold.processing_flags

        # If there is no result
        if "resultImg" not in cache:
            for k in self.dataframe.columns:
                data[k] = '-'
            data['Filename'] = img_name
            data['comment'] = "REJECTED"
        else:
            failed = False
            # Get center from ImageData (reference center in original coordinates)
            # Priority: ImageData.center > orig_image_center > (0,0)
            if hasattr(quadFold, '_image_data') and quadFold._image_data is not None:
                # Use ImageData.center (the reference center, handles manual/auto)
                center = quadFold._image_data.center
            elif quadFold.orig_image_center is not None:
                center = (round(quadFold.orig_image_center[0], 2), round(quadFold.orig_image_center[1], 2))
            else:
                # Should never happen, but provide a safe fallback
                center = (0, 0)
            # Get all needed infos
            data['Filename'] = img_name
            data['version'] = self.version
            data['date'] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            data['centerX'] = center[0]
            data['centerY'] = center[1]
            data['rotationAngle'] = quadFold.rotation if quadFold.rotation is not None else 0.0
            # try:
            #     data['hash'] = hashlib.sha512(cache['resultImg']).hexdigest()
            # except:
            #     print('Hash not generated, array not C-contiguous')
            #     data['hash'] = '-'

            data['backgroundMethod'] = quadFold.info['result_bg'].get('method', '-')
            data['backgroundConfigName'] = quadFold.info['result_bg'].get('selected_configuration_name', '-')
            data['parameters'] = quadFold.info['result_bg'].get('final_params', '-')
            data['downsampled'] = quadFold.info.get('downsample', '-')
            data['loss'] = quadFold.info['result_bg'].get('loss', '-')
            data['bgSum'] = quadFold.info['result_bg'].get('intensity', '-')
            data['symmetry'] = quadFold.info['result_bg'].get('symmetry', '-')

            # Blank/mask + calibration settings, read off the ImageData
            # (which carries the resolved SettingsManager state). Same
            # source in GUI and headless, so the values stay consistent.
            data.update(self._blankMaskCalibrationColumns(quadFold))

            data = data | processed_flags

            # fixed_roi_w/h is the persisted ROI preference, but the
            # pipeline consumes it under roi_w/roi_h (headless getFlags
            # renames fixed_roi_* -> roi_* before processing). Map the
            # value back so the persisted setting is recorded under its
            # canonical fixed_roi_* column instead of being dropped.
            if 'roi_w' in processed_flags:
                data['fixed_roi_w'] = processed_flags['roi_w']
            if 'roi_h' in processed_flags:
                data['fixed_roi_h'] = processed_flags['roi_h']

            if failed:
                self.failedcases.add(img_name)
            elif img_name in self.failedcases:
                self.failedcases.remove(img_name)

        self.dataframe = pd.concat([self.dataframe, pd.DataFrame.from_records([data])])
        # self.dataframe = self.dataframe.append(data, ignore_index=True) # Future warning deprecated
        self.dataframe.reset_index()
        self.dataframe.to_csv(self.filename, index=False, columns=self.colnames) # Write to csv file

    def _blankMaskCalibrationColumns(self, quadFold):
        """Pull blank/mask + calibration settings off the QuadrantFolder's
        ImageData. Returns a dict with '-' for any value that cannot be
        resolved (no ImageData / no SettingsManager / no calibration)."""
        cols = {
            'blank_enabled': '-',
            'mask_enabled': '-',
            'blank_weight': '-',
            'detector': '-',
            'lambda_sdd': '-',
        }
        image_data = getattr(quadFold, '_image_data', None)
        if image_data is None:
            return cols

        cols['blank_enabled'] = bool(getattr(image_data, 'apply_blank', False))
        cols['mask_enabled'] = bool(getattr(image_data, 'apply_mask', False))
        cols['blank_weight'] = getattr(image_data, 'blank_weight', '-')

        detector = getattr(image_data, 'detector', None)
        if detector:
            cols['detector'] = detector

        sm = getattr(image_data, '_settings_manager', None)
        if sm is not None:
            try:
                derived = sm.derive_processing_calibration()
            except Exception:
                derived = {}
            if 'lambda_sdd' in derived:
                cols['lambda_sdd'] = derived['lambda_sdd']
            if cols['detector'] == '-' and derived.get('detector'):
                cols['detector'] = derived['detector']
        return cols

    def removeData(self, img_name):
        """
        Remove data from dataframe
        :param img_name: (str)
        :return:
        """
        self.dataframe = self.dataframe[self.dataframe["Filename"] != img_name]
        
    def sortCSV(self):
        if exists(self.filename):
            df = pd.read_csv(self.filename)
            df.sort_values(by='Filename', inplace=True)
            df.to_csv(self.filename, index=False)
