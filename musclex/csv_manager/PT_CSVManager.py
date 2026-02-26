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
import numpy as np
try:
    from ..utils.file_manager import fullPath, createFolder
except: # for coverage
    from utils.file_manager import fullPath, createFolder

class PT_CSVManager:
    """
    A class taking care of writing results including csv file of Projection Traces
    """
    def __init__(self, dir_path, boxes):
        """
        init with directory path
        :param dir_path: directory path
        :param boxes: Dict[str, ProcessingBox] - box objects with all configuration
        """
        self.dataframe = None
        result_path = fullPath(dir_path, "pt_results")
        createFolder(result_path)
        self.filename = fullPath(result_path, 'summary.csv')
        self.setColumnNames(boxes=boxes)
        self.loadSummary()

    def setColumnNames(self, boxes):
        """
        Set Column names from ProcessingBox objects
        e.g. "Filename", "L1 Meridian Sigma", "L1 Meridian Amplitude", "L1 centroid 0", "L2 Meridian Sigma",...
        :param boxes: Dict[str, ProcessingBox] - box objects with peaks and configuration
        :return:
        """
        self.colnames = ["Filename"]
        for box_name, box in boxes.items():
            if box.peaks:
                self.colnames.append("Box " + str(box_name) + " Meridian Sigma")
                self.colnames.append("Box " + str(box_name) + " Meridian Area")
                for i in range(len(box.peaks)):
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
                self.colnames.append ("Box " + str(box_name) + " Background Sigma")
                self.colnames.append ("Box " + str(box_name) + " Background Amplitude")
                self.colnames.append ("Box " + str(box_name) + " Meridian Background Sigma")
                self.colnames.append ("Box " + str(box_name) + " Meridian Background Amplitude")
                self.colnames.append ("Box " + str(box_name) + " Meridian Amplitude")
                self.colnames.append("Box " + str(box_name) + " error")
                self.colnames.append("Box " + str(box_name) + " comments")
        
        # Global reject column (used for image-level reject status)
        self.colnames.append("reject")
        
        # Global comments column (after reject, last column)
        self.colnames.append("comments")

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
        :param projProc: Projection Processor object with results
        :return: -
        """
        file_name = projProc.filename
        self.removeData(file_name)
        new_data = {
            'Filename' : file_name
        }

        for bn, box in projProc.boxes.items():
            if box.fit_results is not None:
                model = box.fit_results
                new_data['Box ' + str(bn) + ' Meridian Sigma'] = model['center_sigma2']
                new_data['Box ' + str(bn) + ' Meridian Area'] = model['center_amplitude2']
                if box.centroids is not None:
                    centroids = box.centroids
                    moved_peaks = np.array(box.moved_peaks) - model['centerX']
                    n_peaks = len(centroids)
                    n_per_side = n_peaks // 2  # First half = right, second half = left (from _expand_peaks_mirrored)
                    for i, c in enumerate(centroids):
                        is_right = i < n_per_side
                        side = " right" if is_right else " left"
                        peak_num = i if is_right else (i - n_per_side)
                        new_data["Box " + str(bn) + " Maximum Point " + str(peak_num) + side + " (Pixel)"] = moved_peaks[i]
                        new_data["Box " + str(bn) + " Centroid " + str(peak_num) + side + " (Pixel)"] = c
                        new_data["Box " + str(bn) + " Centroid Area " + str(peak_num) + side] = box.areas[i]
                        new_data["Box " + str(bn) + " Gaussian Peak " + str(peak_num) + side + " (Pixel)"] = model['p_'+str(i)]
                        new_data["Box " + str(bn) + " Gaussian Sigma " + str(peak_num) + side] = model['sigma'+str(i)]
                        new_data["Box " + str(bn) + " Gaussian Area " + str(peak_num) + side] = model['amplitude'+str(i)]
                        if projProc.state.lambda_sdd is not None:
                            new_data["Box " + str(bn) + " Maximum Point " + str(peak_num) + side + " (nm)"] = projProc.state.lambda_sdd/moved_peaks[i]
                            new_data["Box " + str(bn) + " Centroid " + str(peak_num) + side + " (nm)"] = projProc.state.lambda_sdd/c
                            new_data["Box " + str(bn) + " Gaussian Peak " + str(peak_num) + side + " (nm)"] = projProc.state.lambda_sdd / model['p_' + str(i)]
                        if not is_right:
                            mirror_idx = peak_num  # Right peak at same peak_num
                            new_data["Box " + str(bn) + " Average Centroid Area " + str(peak_num)] = (box.areas[i] + box.areas[mirror_idx])/2
                            new_data["Box " + str(bn) + " Average Gaussian Area " + str(peak_num)] = (model['amplitude'+str(i)] + model['amplitude'+str(mirror_idx)])/2
                new_data["Box " + str(bn) + " error"] = model['error']
                if model['error'] > 0.15:
                    new_data["Box " + str(bn) + " comments"] = "High fitting error"

                new_data["Box " + str(bn) + " Background Sigma"] = model['bg_sigma']
                new_data["Box " + str(bn) + " Background Amplitude"] = model['bg_amplitude']
                new_data["Box " + str(bn) + " Meridian Background Sigma"] = model['center_sigma1']
                new_data["Box " + str(bn) + " Meridian Background Amplitude"] = model['center_amplitude1']
                new_data["Box " + str(bn) + " Meridian Sigma"] = model['center_sigma2']
                new_data["Box " + str(bn) + " Meridian Amplitude"] = model['center_amplitude2']

        # Global reject: write reject status from ProcessingState
        if projProc.state.rejected:
            new_data["reject"] = "rejected"

        # Global comments: write comments from ProcessingState
        comments_text = projProc.state.comments.strip()
        if comments_text:
            new_data["comments"] = comments_text

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
