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
import hashlib
import pandas as pd
try:
    from ..utils.file_manager import fullPath
except: # for coverage
    from utils.file_manager import fullPath

class QF_CSVManager:
    """
    A class taking care of writing results including csv file and failedcases file
    """
    def __init__(self, dir_path):
        """
        init with directory path
        :param dir_path:
        """
        self.dataframe = None
        result_path = fullPath(dir_path, "qf_results")
        if not exists(result_path):
            makedirs(result_path)
        self.filename = fullPath(result_path, 'summary.csv')
        self.colnames = ['Filename', 'centerX', 'centerY', 'rotationAngle', 'hash', 'comment']
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

        # If there is no result
        if "resultImg" not in cache:
            for k in self.dataframe.columns:
                data[k] = '-'
            data['Filename'] = img_name
            data['comment'] = "REJECTED"
        else:
            failed = False
            # Priority: fixedCenter > quadFold.center (manual or auto) > orig_image_center > (0,0)
            if quadFold.fixedCenterX is not None and quadFold.fixedCenterY is not None:
                center = [quadFold.fixedCenterX, quadFold.fixedCenterY]
            elif quadFold.center is not None:
                # Use the actual center (whether manual or auto)
                center = quadFold.center
            elif quadFold.orig_image_center is not None:
                center = (round(quadFold.orig_image_center[0], 2), round(quadFold.orig_image_center[1], 2))
            else:
                # Should never happen, but provide a safe fallback
                center = (0, 0)
            # Get all needed infos
            data['Filename'] = img_name
            data['centerX'] = center[0]
            data['centerY'] = center[1]
            data['rotationAngle'] = quadFold.info['rotationAngle']
            try:
                data['hash'] = hashlib.sha512(cache['resultImg']).hexdigest()
            except:
                print('Hash not generated, array not C-contiguous')
                data['hash'] = '-'

            if failed:
                self.failedcases.add(img_name)
            elif img_name in self.failedcases:
                self.failedcases.remove(img_name)

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
        
    def sortCSV(self):
        if exists(self.filename):
            df = pd.read_csv(self.filename)
            df.sort_values(by='Filename', inplace=True)
            df.to_csv(self.filename, index=False)
