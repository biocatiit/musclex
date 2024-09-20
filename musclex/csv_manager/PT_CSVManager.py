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
try:
    from ..utils.file_manager import fullPath, createFolder
except: # for coverage
    from utils.file_manager import fullPath, createFolder

class PT_CSVManager:
    """
    A class taking care of writing results including csv file of Projection Traces
    """
    def __init__(self, dir_path, boxes, peaks):
        """
        init with directory path
        :param dir_path:
        """
        self.dataframe = None
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
                    side = " right" if i%2 == 0 else " left"
                    self.colnames.append("Box " + str(box_name) + " Maximum Point " + str(i//2) + side + " (Pixel)")
                    self.colnames.append("Box " + str(box_name) + " Maximum Point " + str(i//2) + side + " (nm)")
                    self.colnames.append("Box " + str(box_name) + " Centroid " + str(i//2) + side + " (Pixel)")
                    self.colnames.append("Box " + str(box_name) + " Centroid " + str(i//2) + side + " (nm)")
                    self.colnames.append("Box " + str(box_name) + " Centroid Area " + str(i//2) + side)
                    self.colnames.append("Box " + str(box_name) + " Gaussian Peak " + str(i//2) + side + " (Pixel)")
                    self.colnames.append("Box " + str(box_name) + " Gaussian Peak " + str(i//2) + side + " (nm)")
                    self.colnames.append("Box " + str(box_name) + " Gaussian Sigma " + str(i//2) + side)
                    self.colnames.append("Box " + str(box_name) + " Gaussian Area " + str(i//2) + side)
                    if i%2 == 1:
                        self.colnames.append("Box " + str(box_name) + " Average Centroid Area " + str(i//2))
                        self.colnames.append("Box " + str(box_name) + " Average Gaussian Area " + str(i//2))
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
                        side = " right" if i%2 == 0 else " left"
                        new_data["Box " + str(bn) + " Maximum Point " + str(i//2) + side + " (Pixel)"] = moved_peaks[i]
                        new_data["Box " + str(bn) + " Centroid " + str(i//2) + side + " (Pixel)"] = c
                        new_data["Box " + str(bn) + " Centroid Area " + str(i//2) + side] = info["areas"][bn][i]
                        new_data["Box " + str(bn) + " Gaussian Peak " + str(i//2) + side + " (Pixel)"] = model['p_'+str(i)]
                        new_data["Box " + str(bn) + " Gaussian Sigma " + str(i//2) + side] = model['sigma'+str(i)]
                        new_data["Box " + str(bn) + " Gaussian Area " + str(i//2)+ side] = model['amplitude'+str(i)]
                        if 'lambda_sdd' in info:
                            new_data["Box " + str(bn) + " Maximum Point " + str(i//2) + side + " (nm)"] = info['lambda_sdd']/moved_peaks[i]
                            new_data["Box " + str(bn) + " Centroid " + str(i//2) + side + " (nm)"] = info['lambda_sdd']/c
                            new_data["Box " + str(bn) + " Gaussian Peak " + str(i//2) + side + " (nm)"] = info['lambda_sdd'] / model['p_' + str(i)]
                        if i%2 == 1:
                            new_data["Box " + str(bn) + " Average Centroid Area " + str(i//2)] = (info["areas"][bn][i] + info["areas"][bn][i-1])/2
                            new_data["Box " + str(bn) + " Average Gaussian Area " + str(i//2)] = (model['amplitude'+str(i)] + model['amplitude'+str(i-1)])/2
                new_data["Box " + str(bn) + " error"] = model['error']
                if model['error'] > 0.15:
                    new_data["Box " + str(bn) + " comments"] = "High fitting error"

        for col in self.colnames:
            if col not in new_data:
                new_data[col] = '-'

        self.dataframe = pd.concat([self.dataframe, pd.DataFrame.from_records([pd.Series(new_data)])])
        # self.dataframe = self.dataframe.append(pd.Series(new_data), ignore_index=True) # Future warning deprecated
        self.dataframe.reset_index()
        self.dataframe.to_csv(self.filename, index=False, columns=self.colnames) # Write to csv file

    def removeData(self, file_name):
        """
        Remove data from dataframe
        :param file_name: (str)
        :return:
        """
        self.dataframe = self.dataframe[self.dataframe["Filename"] != file_name]
        
    def sortCSV(self):
        if exists(self.filename):
            df = pd.read_csv(self.filename)
            df.sort_values(by='Filename', inplace=True)
            df.to_csv(self.filename, index=False)
