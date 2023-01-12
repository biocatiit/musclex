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
import pandas as pd
try:
    from ..utils.file_manager import fullPath
except: # for coverage
    from utils.file_manager import fullPath

class XV_CSVManager:
    """
    A class taking care of writing results including csv file and failedcases file
    """
    def __init__(self, dir_path):
        """
        init with directory path
        :param dir_path:
        """
        self.dataframe = None
        self.result_path = fullPath(dir_path, "xv_results")
        self.filename = fullPath(self.result_path, 'summary.csv')
        self.colnames = ['Filename', 'Histogram', 'Comment']
        self.loadSummary()

    def loadSummary(self):
        """
        Load summary.csv file and keep data in self.dataframe
        :return:
        """
        if not exists(self.filename):
            self.dataframe = pd.DataFrame(columns = self.colnames)
        else:
            self.dataframe = pd.read_csv(self.filename)

    def writeNewData(self, xrayViewer):
        """
        Add new data to dataframe, then re-write summary.csv
        :param xrayViewer: QuadrantFolder object with results in its info dict
        :return: -
        """
        if not exists(self.result_path):
            makedirs(self.result_path)
        img_name = xrayViewer.img_name
        self.removeData(img_name)
        data = {}

        # If there is no result
        if xrayViewer.hist == []:
            for k in self.dataframe.columns:
                data[k] = '-'
            data['Filename'] = img_name
            data['comment'] = "No slice or box selected"
        else:
            # Get all needed infos
            data['Filename'] = img_name
            data['Histogram'] = xrayViewer.hist

        self.dataframe = pd.concat([self.dataframe, pd.DataFrame.from_records([data])])
        # self.dataframe = self.dataframe.append(data, ignore_index=True) # Future warning deprecated
        self.dataframe.reset_index()
        self.dataframe.to_csv(self.filename, index=False, columns=self.colnames) # Write to csv file

    def removeData(self, img_name):
        """
        Remove data from dataframe
        :param img_name: (str)
        :return:
        """
        self.dataframe = self.dataframe[self.dataframe["Filename"] != img_name]