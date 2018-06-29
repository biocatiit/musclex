import os, datetime
from ..ui.pyqt_utils import *

class Logger:

    def __init__(self, name, path='~'):
        '''
        params
            name...program name
            path...
        '''
        self.logger = None
        folder = os.path.expanduser(os.path.join(path, 'log'))
        if not os.path.exists(folder):
            os.mkdir(folder)
        fname = '{0}_{1}_{2}.log'.format(name, os.path.split(path)[-1], datetime.date.today())
        fname = os.sep.join([folder, fname])
        self.logger = open(fname, 'a')

    def write(self, msg):
        print(msg, file=self.logger)

    def popup(self):
        popupMsg = QMessageBox()
        popupMsg.setWindowTitle('New log generated')
        popupMsg.setText('New log generated')
        msg = 'Mannual changes of parameters are recorded in file "%s".\n' % self.logger.name
        msg += 'Please report them to us if you are willing to help us optimize our program.'
        popupMsg.setInformativeText(msg)
        popupMsg.setStandardButtons(QMessageBox.Ok)
        popupMsg.setIcon(QMessageBox.Information)
        popupMsg.setFixedWidth(1000)
        popupMsg.exec_()

    def close(self):
        if self.logger is not None:
            self.logger.close()