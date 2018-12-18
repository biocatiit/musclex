from musclex.ui.ui_launcherform import *
from musclex import __version__
import sys, subprocess, os, os.path
import configparser
from musclex.utils.exception_handler import handlers

if sys.platform in handlers:
    sys.excepthook = handlers[sys.platform]

class LauncherForm(QWidget):

    programs = ['eq', 'qf', 'pt', 'di', 'im', 'dc', 'ddf']

    def __init__(self):
        super(QWidget, self).__init__()
        
        # Set up the user interface from Designer.
        self.ui = Ui_LauncherForm()
        self.ui.setupUi(self)
        self.setWindowTitle("MuscleX Launcher v" + __version__)

        # Set up popup message box
        popupMsg = QMessageBox()
        popupMsg.setWindowTitle('Note')
        popupMsg.setTextFormat(Qt.RichText)
        popupMsg.setText(
"""Please help us impove our program by reporting exceptions or bugs to
<a href="https://www.github.com/biocatiit/musclex/issues">
https://www.github.com/biocatiit/musclex/issues</a>.""")
        popupMsg.setInformativeText(
"""When reporting, besides complete error logs, we hope you can also provide"""
"""the information of your platfrom and images you're processing. """)
        popupMsg.setIcon(QMessageBox.Information)
        popupMsg.setCheckBox(QCheckBox("Do not show this again.", self))
        pmlayout = popupMsg.layout()
        pmlayout.addItem(QSpacerItem(756, 0), pmlayout.rowCount(), 0, 1, pmlayout.columnCount())
        self.popupMsg = popupMsg
        
        # Make some local initializations.
        self.program_idx = 0
        self.ui.runButton.clicked.connect(self.launch)
        self.ui.stackedWidget.currentChanged['int'].connect(self.select)

        # Read the config file
        config = configparser.RawConfigParser()
        config.optionxform = lambda option: option
        ininame = os.path.join(os.path.expanduser('~'), 'musclex.ini')
        print('Config file at ' + ininame)
        if os.path.exists(ininame):
            config.read(ininame)
            if 'Launcher' in config and 'ShowMessage' in config['Launcher']:
                self.popupMsg.checkBox().setChecked(not config['Launcher'].getboolean('ShowMessage'))
        else:
            open(ininame, 'a').close()
        self.config = config
        self.ininame = ininame
        
    def select(self, idx):
        self.program_idx = idx

    def launch(self):
        prog = LauncherForm.programs[self.program_idx]
        try:
            path = os.path.dirname(sys.argv[0])
            path = '.' if path == '' else path
            subprocess.Popen([os.path.join(path, 'musclex-main'), prog], 
            	shell=(sys.platform=='win32'))
        except FileNotFoundError:
            subprocess.Popen(['musclex', prog], shell=(sys.platform=='win32'))

    def keyReleaseEvent(self, event):
        if event.key() == Qt.Key_Return:
            self.launch()

    def closeEvent(self, event):
        if not self.popupMsg.checkBox().isChecked():
            self.popupMsg.exec_()
            if self.popupMsg.checkBox().isChecked():
                if 'Launcher' not in self.config:
                    self.config['Launcher'] = {}
                if 'ShowMessage' not in self.config['Launcher']:
                    self.config['Launcher']['ShowMessage'] = str(1)
                self.config['Launcher']['ShowMessage'] = str(0)
                with open(self.ininame, 'w') as configfile:
                    self.config.write(configfile)

    @staticmethod
    def main():
        app = QApplication(sys.argv)
        window = LauncherForm()
        window.show()
        sys.exit(app.exec_())


if __name__ == "__main__":
    LauncherForm.main()
    
