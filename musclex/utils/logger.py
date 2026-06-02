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

from __future__ import print_function
import os
import datetime
import time
from ..ui.pyqt_utils import *

class Logger:
    """
    Logger
    """
    def __init__(self, name, path='~'):
        '''
        params
            name...program name
            path...
        '''
        self.logger = None
        self.name = name
        folder = os.path.expanduser(os.path.join(path, 'log'))
        if not os.path.exists(folder):
            os.mkdir(folder)
        fname = f'{name}_{os.path.split(path)[-1]}_{datetime.date.today()}.log'
        fname = os.sep.join([folder, fname])
        self.logger = open(fname, 'a')

    def write(self, msg):
        """
        Write a message in the logger with the time
        """
        print(time.asctime(), msg, file=self.logger)

    def popup(self):
        """
        Open a Qt pop up window
        """
        popupMsg = QMessageBox()
        popupMsg.setWindowTitle('New log generated')
        text = 'The log file is stored with the images.\n\n'
        text += f'Mannual changes of parameters are recorded in file "{self.logger.name}".\n'
        text += 'Please report them to us if you are willing to help us optimize our program.'
        popupMsg.setText(text)
        popupMsg.setStandardButtons(QMessageBox.Ok)
        popupMsg.setIcon(QMessageBox.Information)
        try:
            popupMsg.setTextInteractionFlags(Qt.TextSelectableByMouse)
        except AttributeError:
            pass # not supported in Qt4
        popupMsg.setMinimumWidth(1000)
        popupMsg.exec_()

    def close(self):
        """
        Close the logger file
        """
        if self.logger is not None:
            self.logger.close()
