from PyQt5.QtWidgets import QApplication, QWidget
from PyQt5.QtCore import Qt
from musclex.ui.ui_launcherform import Ui_LauncherForm
import sys, subprocess, os.path

class LauncherForm(QWidget):

    programs = ['eq', 'qf', 'pt', 'di', 'im', 'dc', 'ddf']

    def __init__(self):
        super(QWidget, self).__init__()
        
        # Set up the user interface from Designer.
        self.ui = Ui_LauncherForm()
        self.ui.setupUi(self)
        
        # Make some local initializations.
        self.program_idx = 0
        self.ui.runButton.clicked.connect(self.launch)
        self.ui.stackedWidget.currentChanged['int'].connect(self.select)
        
    def select(self, idx):
        self.program_idx = idx

    def launch(self):
        prog = LauncherForm.programs[self.program_idx]
        try:
            path = os.path.dirname(sys.argv[0])
            path = '.' if path == '' else path
            subprocess.Popen(os.path.join(path, 'musclex ') + prog, shell=True)
        except FileNotFoundError:
            subprocess.Popen('musclex ' + prog, shell=True)

    def keyReleaseEvent(self, event):
        if event.key() == Qt.Key_Return:
            self.launch()
    
    @staticmethod
    def main():
        app = QApplication(sys.argv)
        window = LauncherForm()
        window.show()
        sys.exit(app.exec_())


if __name__ == "__main__":
    LauncherForm.main()
    