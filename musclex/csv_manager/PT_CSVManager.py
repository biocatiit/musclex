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
from os.path import exists
import pandas as pd
from ..utils.file_manager import fullPath, createFolder

class PT_CVSManager:
    """
    A class taking care of writing results including csv file of Projection Traces
    """
    def __init__(self, dir_path, boxes, peaks):
        """
        init with directory path
        :param dir_path:
        """
        result_path = fullPath(dir_path, "pt_results")
        createFolder(result_path)
        self.filename = fullPath(result_path, 'summary.csv')
        self.setColumnNames(boxes=boxes, peaks=peaks)
        self.loadSummary()

    def setColumnNames(self, boxes, peaks):
        """
        Set Colume name
        e.g. "Filename", "L1 Meridian Sigma", "L1 Meridian Amplitude", "L1 centroid 0", "L2 Meridian Sigma",...
        :param boxes: boxes dictionary (box_name:box coords)
        :param peaks: peaks dictionary (box_name:peak list)
        :return:
        """
        self.colnames = ["Filename"]
        for box_name in boxes.keys():
            if box_name in peaks:
                self.colnames.append("Box " + str(box_name) + " Meridian Sigma")
                self.colnames.append("Box " + str(box_name) + " Meridian Area")
                for i in range(len(peaks[box_name])):
                    self.colnames.append("Box " + str(box_name) + " Maximum Point " + str(i) + " (Pixel)")
                    self.colnames.append("Box " + str(box_name) + " Maximum Point " + str(i) + " (nm)")
                    self.colnames.append("Box " + str(box_name) + " Centroid " + str(i) + " (Pixel)")
                    self.colnames.append("Box " + str(box_name) + " Centroid " + str(i) + " (nm)")
                    self.colnames.append("Box " + str(box_name) + " Gaussian Peak " + str(i) + " (Pixel)")
                    self.colnames.append("Box " + str(box_name) + " Gaussian Peak " + str(i) + " (nm)")
                    self.colnames.append("Box " + str(box_name) + " Gaussian Sigma " + str(i))
                    self.colnames.append("Box " + str(box_name) + " Gaussian Area " + str(i))
                self.colnames.append("Box " + str(box_name) + " error")
                self.colnames.append("Box " + str(box_name) + " comments")

    def loadSummary(self):
        """
        Load summary.csv file and keep data in self.dataframe
        :return:
        """
        if not exists(self.filename):
            self.dataframe = pd.DataFrame(columns = self.colnames)
        else:
            self.dataframe = pd.read_csv(self.filename)
        # print self.dataframe

    def writeNewData(self, projProc):
        """
        Add new data to dataframe, then re-write summary.csv
        :param projProc: Projection Processor object with results in its info dict
        :return: -
        """
        file_name = projProc.filename
        info = projProc.info
        self.removeData(file_name)
        new_data = {
            'Filename' : file_name
        }

        box_names = info['box_names']
        for bn in box_names:
            if 'fit_results' in info and bn in info['fit_results']:
                model = info['fit_results'][bn]
                new_data['Box ' + str(bn) + ' Meridian Sigma'] = model['center_sigma2']
                new_data['Box ' + str(bn) + ' Meridian Area'] = model['center_amplitude2']
                if 'centroids' in info and bn in info['centroids']:
                    centroids = info['centroids'][bn]
                    moved_peaks = info['moved_peaks'][bn] - model['centerX']
                    for i,c in enumerate(centroids):
                        new_data["Box " + str(bn) + " Maximum Point " + str(i) + " (Pixel)"] = moved_peaks[i]
                        new_data['Box ' + str(bn) + " Centroid " + str(i) + " (Pixel)"] = c
                        new_data["Box " + str(bn) + " Gaussian Peak " + str(i) + " (Pixel)"] = model['p_'+str(i)]
                        new_data["Box " + str(bn) + " Gaussian Sigma " + str(i)] = model['sigma'+str(i)]
                        new_data["Box " + str(bn) + " Gaussian Area " + str(i)] = model['amplitude'+str(i)]
                        if 'lambda_sdd' in info:
                            new_data["Box " + str(bn) + " Maximum Point " + str(i) + " (nn)"] = 1.*info['lambda_sdd']/moved_peaks[i]
                            new_data['Box ' + str(bn) + " Centroid " + str(i) + " (nn)"] = 1.*info['lambda_sdd']/c
                            new_data["Box " + str(bn) + " Gaussian Peak " + str(i) + " (nn)"] = 1. * info['lambda_sdd'] / model['p_' + str(i)]
                new_data["Box " + str(bn) + " error"] = model['error']
                if model['error'] > 0.15:
                    new_data["Box " + str(bn) + " comments"] = "High fitting error"

        for col in self.colnames:
            if col not in new_data:
                new_data[col] = '-'

        self.dataframe = self.dataframe.append(pd.Series(new_data), ignore_index=True)
        self.dataframe.reset_index()
        self.dataframe.to_csv(self.filename, index=False, columns=self.colnames) # Write to csv file

    def removeData(self, file_name):
        """
        Remove data from dataframe
        :param file_name: (str)
        :return:
        """
        self.dataframe = self.dataframe[self.dataframe["Filename"] != file_name]
