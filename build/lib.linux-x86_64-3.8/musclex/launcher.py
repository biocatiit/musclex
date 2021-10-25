from musclex.ui.ui_launcherform import *
from musclex import __version__
import sys, subprocess, os, os.path
sys.path.append('..')
import configparser
import unittest
import time
from musclex.utils.exception_handler import handlers

from musclex.tests.module_test import *

if sys.platform in handlers:
    sys.excepthook = handlers[sys.platform]

class LauncherForm(QWidget):

    programs = ['eq', 'qf', 'pt', 'di', 'im', 'dc', 'ddf', 'ai']

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
        self.ui.testButton.clicked.connect(self.test)
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
        self.test_path = os.path.join(os.path.dirname(__file__),
                                       "tests", "test_logs", "test.log")

        #if not os.path.exists(self.test_path):
            # self.testPopup = QMessageBox()
            # self.testPopup.setWindowTitle('Testing')
            # self.testPopup.setTextFormat(Qt.RichText)
            # self.testPopup.setText('No test log found. A test of the MuscleX installation will be run.')
            # self.testPopup.setInformativeText('Press OK to begin the tests. This could take several minutes..')
            # self.testPopup.setIcon(QMessageBox.Information)
            # self.testLayout = self.testPopup.layout()
            # self.testLayout.addItem(QSpacerItem(756, 0), self.testLayout.rowCount(), 0, 1, self.testLayout.columnCount())
            # self.testPopup.show()
            # QApplication.processEvents()
            # self.test()
        QApplication.processEvents()
    def select(self, idx):
        self.program_idx = idx

    def launch(self):
        prog = LauncherForm.programs[self.program_idx]
        try:
            path = os.path.dirname(sys.argv[0])
            path = '.' if path == '' else path
            subprocess.Popen([os.path.join(path, 'musclex-main'), prog],
            	shell=(sys.platform=='win32'))
        except IOError:
            subprocess.Popen(['musclex', prog], shell=(sys.platform=='win32'))

    def test(self):
        self.td = TestDialog()
        self.td.show()
        self.td.activateWindow()
        self.td.raise_()
        if not os.path.exists(self.test_path):
            self.td.run_test()

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

class TestDialog(QDialog):
    """
    Qt Class definition for the TestDialog window.
    """
    def __init__(self):
        super(QWidget, self).__init__()
        self.setWindowFlags(Qt.WindowStaysOnTopHint)
        # Fixed path to the test log
        self.test_path = os.path.join(os.path.dirname(__file__), "tests", "test_logs", "test.log")
        self.green = QColor(0,150,0)
        self.red = QColor(150,0,0)
        self.black = QColor(0,0,0)
        self.initUI()

    def initUI(self):
        self.testDialogLayout = QVBoxLayout()
        self.runTestsButton = QPushButton('Run Tests')
        self.runGPUTestButton = QPushButton('Run GPU Test')
        self.showLatestTestButton = QPushButton('Show Latest Test Results')
        self.cancelButton = QPushButton('Cancel')

        self.progressBar = QProgressBar(self)
        self.progressBar.setGeometry(0, 0, 300, 25)
        self.progressBar.setMaximum(100)

        self.testDialogLayout.addWidget(self.runTestsButton)
        self.testDialogLayout.addWidget(self.runGPUTestButton)
        self.testDialogLayout.addWidget(self.showLatestTestButton)
        self.testDialogLayout.addWidget(self.cancelButton)
        self.testDialogLayout.addWidget(self.progressBar)

        self.runTestsButton.clicked.connect(self.runTestsButtonClicked)
        self.runGPUTestButton.clicked.connect(self.runGPUTestButtonClicked)
        self.showLatestTestButton.clicked.connect(self.showLatestTestButtonClicked)
        self.cancelButton.clicked.connect(self.cancelButtonClicked)

        self.setLayout(self.testDialogLayout)
        self.resize(700,500)

        self.detail = QTextEdit()
        self.detail.setReadOnly(True)
        self.detail.setFontWeight(100)
        if os.path.exists(self.test_path):
            self.detail.insertPlainText("Module tests have already been run.\nPress \'Run Tests\' to run the module tests again.")
            self.detail.insertPlainText("\n\nTest results:\n{}{}{}\nSee the log at {} for more info.\n"
                                        .format('-'*80,self.get_latest_test(),'-'*80, self.test_path))
        else:
            self.detail.insertPlainText("No test logs found. Running unit tests for the first time..\n")

        self.testDialogLayout.addWidget(self.detail)
        QApplication.processEvents()
        self.detail.setFontWeight(50)
        self.detail.moveCursor(QTextCursor.Start)
        QApplication.processEvents()


    def cancelButtonClicked(self):
        self.close()

    def runTestsButtonClicked(self):
        self.run_test()

    def runGPUTestButtonClicked(self):
        """
        Run GPU Tests from unittest.
        """
        self.progressBar.reset()
        self.detail.moveCursor(QTextCursor.End)
        QApplication.processEvents()

        suite = unittest.TestSuite()
        suite.addTest(MuscleXTest("testOpenCLDevice"))
        suite.addTest(MuscleXTest("testGPUIntegratePyFAI"))
        runner = unittest.TextTestRunner()
        runner.run(suite)

        self.detail.setFontWeight(100)
        self.detail.insertPlainText("GPU tests complete.\n")
        self.detail.moveCursor(QTextCursor.NoMove)
        QApplication.processEvents()

        test_results = self.get_latest_test()

        opencl_results = test_results.split('OpenCL GPU Device Test:')
        pyfai_results = test_results.split('pyFAI Integration Test:')

        if len(opencl_results) >= 2:
            opencl_pass = (opencl_results[1][1:5] == 'pass')
        else:
            opencl_pass = False
        if len(pyfai_results) >= 2:
            pyfai_pass = (pyfai_results[1][1:5] == 'pass')
        else:
            pyfai_pass = False
        pass_test = opencl_pass and pyfai_pass

        if pass_test:
            self.detail.setTextColor(self.green)
            self.detail.insertPlainText("Tests Passed -- GPU acceleration is available.\n")
        else:
            self.detail.setTextColor(self.red)
            self.detail.insertPlainText("Tests failed -- GPU acceleration is not available.\n")
        QApplication.processEvents()

        self.detail.setTextColor(self.black)
        self.detail.setFontWeight(50)
        self.detail.insertPlainText("Test results:\n{}{}{}"
                                    .format('-'*80,test_results,'-'*80))
        self.progressBar.setValue(100)
        QApplication.processEvents()

    def showLatestTestButtonClicked(self):
        self.detail.moveCursor(QTextCursor.End)
        QApplication.processEvents()

        self.detail.setFontWeight(100)
        self.detail.insertPlainText("\nLatest test results:\n")
        self.detail.moveCursor(QTextCursor.End)
        QApplication.processEvents()

        self.detail.setFontWeight(50)
        self.detail.insertPlainText("{}{}{}"
                                    .format('-'*80,self.get_latest_test(),'-'*80))
        QApplication.processEvents()

    def run_test(self):
        """
        Run the unittest in a subprocess while monitoring progress
        from the log in the parent process.
        """
        self.progressBar.reset()
        NTESTS = 8
        if getattr(sys, 'frozen', False):
            subproc = subprocess.Popen(os.path.join(os.path.dirname(sys._MEIPASS),"run_tests.sh")) # run the test program in a subprocess
        elif __file__:
            subproc = subprocess.Popen(os.path.join(os.path.dirname(__file__),"run_tests.sh")) # run the test program in a subprocess

        if os.path.exists(self.test_path):
            prev_data = open(self.test_path, 'r').readlines()
        else:
            prev_data = ""

        self.detail.moveCursor(QTextCursor.End)
        self.detail.setFontWeight(100)
        self.detail.insertPlainText("\nRunning tests of MuscleX modules.\nThis could take a few minutes...")
        QApplication.processEvents()

        progress = 0
        test_number = 0
        while progress < 100 and subproc.poll() is None:
            time.sleep(0.5)
            self.detail.moveCursor(QTextCursor.End)
            self.detail.insertPlainText(".")
            QApplication.processEvents()
            if os.path.exists(self.test_path):
                logfile = open(self.test_path, 'r')
                curr_data = logfile.readlines()
                if curr_data != prev_data:
                    test_number += 1
                    progress += 100 / NTESTS
                    self.progressBar.setValue(progress)
                    QApplication.processEvents()

                    self.detail.moveCursor(QTextCursor.End)
                    self.detail.insertPlainText("\nFinished test {} out of {}.\n"
                                                .format(test_number, NTESTS))
                    QApplication.processEvents()
                prev_data = curr_data
            else:
                pass
        self.progressBar.setValue(100)
        QApplication.processEvents()

        self.detail.moveCursor(QTextCursor.End)
        self.detail.setFontWeight(100)
        self.detail.insertPlainText("\nModule tests complete.")
        QApplication.processEvents()

        test_results = self.get_latest_test()
        test_summary = test_results.split('Summary of Test Results')

        if len(test_summary) >= 2:
            if test_summary[1].find('fail') != -1:
                self.detail.setTextColor(self.red)
                self.detail.insertPlainText("\nSome tests failed -- see below for details.\n")
            else:
                self.detail.setTextColor(self.green)
                self.detail.insertPlainText("\nAll tests passed -- see below for details.\n")
        QApplication.processEvents()

        self.detail.setTextColor(self.black)
        self.detail.setFontWeight(50)
        self.detail.insertPlainText("\nTest results:\n{}{}{}\nSee the log at {} for more info."
                                    .format('-'*80,test_results,'-'*80, self.test_path))
        QApplication.processEvents()

    def get_latest_test(self):
        """
        Display the last test run from the test log.
        """
        if os.path.exists(self.test_path):
            file = open(self.test_path, 'r')
        else:
            return ""
        data = file.read()
        idx = 1
        while(idx < 10):
            last_test = data.split('-'*80)[-idx]
            if last_test == '\n':
                idx += 1
            else:
                break
        file.close()
        return last_test

if __name__ == "__main__":
    LauncherForm.main()
