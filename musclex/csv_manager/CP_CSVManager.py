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

from ..modules.ScanningDiffraction import *
import pandas as pd

class CP_CSVManager():
    def __init__(self, dir_path):
        self.df_sum = None
        self.df_rings = None
        result_path = fullPath(dir_path, 'cp_results')
        createFolder(result_path)
        self.sum_file = fullPath(result_path, "summary.csv")
        self.rings_file = fullPath(result_path, "rings.csv")
        self.sum_header = ['filename', 'total intensity (hull)', 'total intensity', 'number of rings']
        self.rings_header = ['filename', 'ring', 'S', 'd', 'peak sigma', 'peak intensity', 'angle', 'angle sigma', 'angle amplitude', 'angle fitting error']
        self.load_all()

    def load_all(self):
        """
        Load summary.csv and rings.csv file and keep data in self.df_sum and self.df_rings
        :return:
        """
        if exists(self.sum_file):
            self.df_sum = pd.read_csv(self.sum_file)
        else:
            self.df_sum = pd.DataFrame(columns=self.sum_header)
        if exists(self.rings_file):
            self.df_rings = pd.read_csv(self.rings_file)
        else:
            self.df_rings = pd.DataFrame(columns=self.rings_header)

    def remove_data(self, filename):
        """
        Remove data from dataframe
        :param file_name: (str)
        :return:
        """
        self.df_sum = self.df_sum[self.df_sum["filename"] != filename]
        self.df_rings = self.df_rings[self.df_rings["filename"] != filename]

    def write_new_data(self, cir_proj):
        """
        Add new data to dataframe, then re-write summary.csv and rings.csv
        :param cir_proj: ScanningDiffraction object with results in its info dict
        :return: -
        """

        file_name = cir_proj.filename
        info = cir_proj.info
        self.remove_data(file_name)

        # Add data to summary.csv
        new_sum_data = {
            'filename' : file_name,
            'total intensity (hull)' : info['area'],
            'total intensity' : info['simple_total_intensity'] if 'simple_total_intensity' in info else info['area']
        }

        if 'model_peaks' in info:
            new_sum_data['number of rings'] = len(info['model_peaks'])

        self.df_sum = self.df_sum.append(new_sum_data, ignore_index = True)
        self.df_sum.reset_index()
        self.df_sum.to_csv(self.sum_file, index=False, columns=self.sum_header)  # Write to csv file

        # Add data to rings.csv
        if 'model_peaks' in info.keys() and len(info['model_peaks']) > 0 and len(info['merged_peaks']) > 0:
            new_datas = []
            nRings = len(info['model_peaks'])
            models = info['ring_models']
            errors = info['ring_errors']
            fit_result = info['fitResult']
            for i in range(nRings):
                if i not in models:
                    continue
                ring = models[i]
                new_data = {'filename':file_name}
                new_data['ring'] = i+1
                new_data['S'] = fit_result['u' + str(i + 1)]
                new_data['peak sigma'] = fit_result['sigmad' + str(i + 1)]
                new_data['peak intensity'] = fit_result['alpha' + str(i + 1)]
                new_data['angle'] = ring['u'] % np.pi
                if '90rotation' in info and info['90rotation']:
                    new_data['angle'] = (ring['u'] + np.pi/2) % np.pi
                new_data['angle sigma'] = ring['sigma']
                new_data['angle amplitude'] = ring['alpha']
                new_data['angle fitting error'] = errors[i]
                if 'peak_ds' in info:
                    new_data['d'] = info['peak_ds'][i]
                else:
                    new_data['d'] = '-'
                new_datas.append(new_data)
            try:
                self.df_rings = self.df_rings.append(new_datas, ignore_index=True)
            except:
                pass
        else:
            for k in self.rings_header:
                new_data = {}
                if k == 'filename':
                    new_data[k] = file_name
                else:
                    new_data[k] = '-'
            self.df_rings = self.df_rings.append(new_data, ignore_index = True)

        self.df_rings.reset_index()
        self.df_rings.to_csv(self.rings_file, index=False, columns=self.rings_header)  # Write to csv file
