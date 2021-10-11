from __future__ import print_function
import os, datetime, time
from ..ui.pyqt_utils import *

class Logger:

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
        fname = '{0}_{1}_{2}.log'.format(name, os.path.split(path)[-1], datetime.date.today())
        fname = os.sep.join([folder, fname])
        self.logger = open(fname, 'a')

    def write(self, msg):
        print(time.asctime(), msg, file=self.logger)

    def popup(self):
        popupMsg = QMessageBox()
        popupMsg.setWindowTitle('New log generated')
        text = 'The log file is stored with the images.\n\n'
        text += 'Mannual changes of parameters are recorded in file "%s".\n' % self.logger.name
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
        if self.logger is not None:
            self.logger.close()
