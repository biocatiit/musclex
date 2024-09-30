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

import configparser
import unittest
import time
import sys
import subprocess
from threading import Thread
import os
import os.path
sys.path.append('..')
from musclex.ui.ui_launcherform import *
from musclex import __version__
from musclex.utils.exception_handler import handlers
from musclex.utils.zip_download import download_zip_pickles
from musclex.tests.module_test import *
from musclex.tests.musclex_tester import MuscleXGlobalTester
from musclex.tests.environment_tester import EnvironmentTester

if sys.platform in handlers:
    sys.excepthook = handlers[sys.platform]

class LauncherForm(QWidget):
    """
    Qt class definition for the GUI launcher
    """
    programs = ['xv', 'eq', 'qf', 'pt', 'di', 'ddf', 'aisme',] # 'dc',

    def __init__(self):
        super().__init__()

        # Set up the user interface from Designer.
        self.ui = Ui_LauncherForm()
        self.ui.setupUi(self)
        self.setWindowTitle("MuscleX Launcher v" + __version__)
        self.td = None

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
        if getattr(sys, 'frozen', False):
            self.test_path = os.path.join(os.path.dirname(sys._MEIPASS), "musclex", "test_logs", "test.log")
            self.release_path = os.path.join(os.path.dirname(sys._MEIPASS), "musclex", "test_logs", "release.log")
        elif __file__:
            self.test_path = os.path.join(os.path.dirname(__file__),
                                        "tests", "test_logs", "test.log")
            self.release_path = os.path.join(os.path.dirname(__file__),
                                       "tests", "test_logs", "release.log")
        QApplication.processEvents()

    def select(self, idx):
        """
        Select a module to launch
        """
        self.program_idx = idx

    def launch(self):
        """
        Launch the selected module
        """
        prog = LauncherForm.programs[self.program_idx]
        try:
            path = os.path.dirname(sys.argv[0])
            path = '.' if path == '' else path
            subprocess.Popen([os.path.join(path, 'musclex-main'), prog],
            	shell=(sys.platform=='win32'))
        except IOError:
            subprocess.Popen(['musclex', prog], shell=(sys.platform=='win32'))

            
            

    def test(self):
        """
        Open tests
        """
        self.td = TestDialog()
        self.td.show()
        self.td.activateWindow()
        self.td.raise_()

    def keyReleaseEvent(self, event):
        """
        Key release event
        """
        if event.key() == Qt.Key_Return:
            self.launch()

    def closeEvent(self, event):
        """
        Close event
        """
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
        """
        Main function for the launcher
        """
        app = QApplication.instance()
        if app is None:
            app = QApplication(sys.argv)
        window = LauncherForm()
        window.show()
        sys.exit(app.exec_())

class TestDialog(QDialog):
    """
    Qt Class definition for the TestDialog window.
    """
    def __init__(self):
        super().__init__()
        # self.setWindowFlags(Qt.WindowStaysOnTopHint)
        # Fixed path to the test log
        if getattr(sys, 'frozen', False):
            self.test_path = os.path.join(os.path.dirname(sys._MEIPASS), "musclex", "test_logs", "test.log")
            self.release_path = os.path.join(os.path.dirname(sys._MEIPASS), "musclex", "test_logs", "release.log")
        elif __file__:
            self.test_path = os.path.join(os.path.dirname(__file__), "tests", "test_logs", "test.log")
            self.release_path = os.path.join(os.path.dirname(__file__), "tests", "test_logs", "release.log")
        self.green = QColor(0,150,0)
        self.red = QColor(150,0,0)
        self.black = QColor(0,0,0)
        self.initUI()

    def initUI(self):
        """
        Initialize the UI
        """
        self.testDialogLayout = QVBoxLayout()
        self.runSummaryTestsButton = QPushButton('Run MuscleX Global Summary Tests')
        self.runDetailedTestsButton = QPushButton('Run MuscleX Detailed Implementation Tests')
        self.runEnvironmentTestButton = QPushButton('Run Environment Test')
        self.runGPUTestButton = QPushButton('Run GPU Test')
        self.showLatestTestButton = QPushButton('Show Latest Test Results')
        self.showReleaseButton = QPushButton('Show Release Results')

        self.progressBar = QProgressBar(self)
        self.progressBar.setGeometry(0, 0, 300, 25)
        self.progressBar.setMaximum(100)

        self.testDialogLayout.addWidget(self.runSummaryTestsButton)
        # self.testDialogLayout.addWidget(self.runDetailedTestsButton)
        self.testDialogLayout.addWidget(self.runEnvironmentTestButton)
        self.testDialogLayout.addWidget(self.runGPUTestButton)
        self.testDialogLayout.addWidget(self.showLatestTestButton)
        self.testDialogLayout.addWidget(self.showReleaseButton)
        self.testDialogLayout.addWidget(self.progressBar)

        self.runSummaryTestsButton.clicked.connect(self.runSummaryTestsButtonClicked)
        self.runDetailedTestsButton.clicked.connect(self.runDetailedTestsButtonClicked)
        self.runEnvironmentTestButton.clicked.connect(self.runEnvTestButtonClicked)
        self.runGPUTestButton.clicked.connect(self.runGPUTestButtonClicked)
        self.showLatestTestButton.clicked.connect(self.showLatestTestButtonClicked)
        self.showReleaseButton.clicked.connect(self.showReleaseButtonClicked)

        self.setLayout(self.testDialogLayout)
        self.resize(700,500)

        self.detail = QTextEdit()
        self.detail.setReadOnly(True)
        self.detail.setFontWeight(100)
        if os.path.exists(self.test_path):
            self.detail.insertPlainText("Module tests have already been run.\nPress \'Run Tests\' to run the module tests again.")
            self.detail.insertPlainText(f"\n\nTest results:\n{'-'*80}{self.get_latest_test()}{'-'*80}\nSee the log at {self.test_path} for more info.\n")
        else:
            self.detail.insertPlainText("No test logs found. Running tests for the first time..\n")

        self.testDialogLayout.addWidget(self.detail)
        QApplication.processEvents()
        self.detail.setFontWeight(50)
        self.detail.moveCursor(QTextCursor.Start)
        QApplication.processEvents()

    def runSummaryTestsButtonClicked(self):
        """
        Triggered when Global summary test button is clicked
        """
        self.run_summary_test()

    def runDetailedTestsButtonClicked(self):
        """
        Triggered when Detailed implementation test button is clicked
        """
        self.run_download_pickles()
        self.run_detailed_test()

    def runEnvTestButtonClicked(self):
        """
        Run Environment Tests.
        """
        self.progressBar.reset()
        self.detail.moveCursor(QTextCursor.End)
        self.detail.setFontWeight(100)
        self.detail.insertPlainText("\nRunning environment tests of MuscleX modules.\nThis will take a few seconds...")
        QApplication.processEvents()

        suite = unittest.TestSuite()
        suite.addTest(EnvironmentTester("testEnvironment"))
        runner = unittest.TextTestRunner()
        proc = Thread(target=runner.run, args=(suite,))
        proc.start()

        self.progressBar.setValue(0)
        QApplication.processEvents()
        while proc.is_alive():
            time.sleep(0.5)
            self.detail.moveCursor(QTextCursor.End)
            self.detail.insertPlainText(".")
            QApplication.processEvents()
        self.progressBar.setValue(100)
        QApplication.processEvents()

        self.detail.moveCursor(QTextCursor.End)
        self.detail.setFontWeight(100)
        self.detail.insertPlainText("\nEnvironment tests complete.")
        QApplication.processEvents()

        test_results = self.get_latest_test()

        if test_results.find('warning') != -1:
            self.detail.setTextColor(self.red)
            self.detail.insertPlainText("\nSome tests failed -- see below for details.\n")
        else:
            self.detail.setTextColor(self.green)
            self.detail.insertPlainText("\nAll tests passed -- see below for details.\n")
        QApplication.processEvents()

        self.detail.setTextColor(self.black)
        self.detail.setFontWeight(50)
        self.detail.insertPlainText(f"Test results:\n{'-'*80}{test_results}{'-'*80}")
        QApplication.processEvents()
        proc.join()

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
        self.detail.insertPlainText(f"Test results:\n{'-'*80}{test_results}{'-'*80}")
        self.progressBar.setValue(100)
        QApplication.processEvents()

    def showLatestTestButtonClicked(self):
        """
        Triggered when the Show lastest test button is clicked
        """
        self.detail.moveCursor(QTextCursor.End)
        QApplication.processEvents()

        self.detail.setFontWeight(100)
        self.detail.insertPlainText("\nLatest test results:\n")
        self.detail.moveCursor(QTextCursor.End)
        QApplication.processEvents()

        self.detail.setFontWeight(50)
        self.detail.insertPlainText(f"{'-'*80}{self.get_latest_test()}{'-'*80}")
        QApplication.processEvents()

    def showReleaseButtonClicked(self):
        """
        Triggered when the Show release button is clicked
        """
        self.detail.moveCursor(QTextCursor.End)
        QApplication.processEvents()

        self.detail.setFontWeight(100)
        self.detail.insertPlainText("\nLatest release results:\n")
        self.detail.moveCursor(QTextCursor.End)
        QApplication.processEvents()

        self.detail.setFontWeight(50)
        self.detail.insertPlainText(f"{'-'*80}{self.get_release_results()}{'-'*80}")
        QApplication.processEvents()

    def run_download_pickles(self):
        """
        Run the downloading for pickle files.
        """
        self.progressBar.reset()
        self.detail.moveCursor(QTextCursor.End)
        self.detail.setFontWeight(100)
        self.detail.insertPlainText("\nDownloading and unzipping pickle files for testing.\nThis could take a few minutes...")
        QApplication.processEvents()

        download_zip_pickles(os.path.dirname(__file__))

        self.progressBar.setValue(100)
        QApplication.processEvents()

        self.detail.moveCursor(QTextCursor.End)
        self.detail.insertPlainText("\nDone.\n")
        QApplication.processEvents()

    def run_summary_test(self):
        """
        Run the gross result testing in a subprocess while monitoring progress
        from the log in the parent process.
        """
        self.progressBar.reset()
        NTESTS = 14

        suite = unittest.TestSuite()
        
        suite.addTest(MuscleXGlobalTester("testHeadlessMarEquator"))
        suite.addTest(MuscleXGlobalTester("testHeadlessEigerEquator"))
        suite.addTest(MuscleXGlobalTester("testHeadlessPilatusEquator"))
        suite.addTest(MuscleXGlobalTester("testHeadlessMarQuadrantFolder"))
        suite.addTest(MuscleXGlobalTester("testHeadlessEigerQuadrantFolder"))
        suite.addTest(MuscleXGlobalTester("testHeadlessPilatusQuadrantFolder"))
        suite.addTest(MuscleXGlobalTester("testHeadlessMarDiffraction"))
        suite.addTest(MuscleXGlobalTester("testHeadlessEigerDiffraction"))
        suite.addTest(MuscleXGlobalTester("testHeadlessPilatusDiffraction"))
        suite.addTest(MuscleXGlobalTester("testHeadlessMarProjectionTraces"))
        suite.addTest(MuscleXGlobalTester("testHeadlessEigerProjectionTraces"))
        suite.addTest(MuscleXGlobalTester("testHeadlessPilatusProjectionTraces"))
        suite.addTest(MuscleXGlobalTester("testHeadlessAISE"))
        runner = unittest.TextTestRunner()
        proc = Thread(target=runner.run, args=(suite,))
        proc.start()

        if os.path.exists(self.test_path):
            prev_data = open(self.test_path, 'r').readlines()
        else:
            prev_data = ""

        self.detail.moveCursor(QTextCursor.End)
        self.detail.setFontWeight(100)
        self.detail.insertPlainText("\nRunning summary tests of MuscleX modules.\nThis could take a few minutes...")
        QApplication.processEvents()

        progress = 0
        test_number = 0
        while progress < 100 and proc.is_alive():
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
                    self.progressBar.setValue(int(progress))
                    QApplication.processEvents()

                    self.detail.moveCursor(QTextCursor.End)
                    self.detail.insertPlainText(f"\nFinished test {test_number} out of {NTESTS}.\n")
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
        if len(test_summary) >= 2:
            self.detail.insertPlainText(f"\nTest results:\n{'-'*80}{test_summary[1]}{'-'*80}\nSee the log at {self.test_path} for more info.")
        QApplication.processEvents()
        proc.join()

    def run_detailed_test(self):
        """
        Run the unittest in a subprocess while monitoring progress
        from the log in the parent process.
        """
        self.progressBar.reset()
        NTESTS = 8

        suite = unittest.TestSuite()
        suite.addTest(MuscleXTest("testEquatorImage"))
        suite.addTest(MuscleXTest("testQuadrantFolder"))
        suite.addTest(MuscleXTest("testDiffractionCentroids"))
        suite.addTest(MuscleXTest("testProjectionTraces"))
        suite.addTest(MuscleXTest("testScanningDiffraction"))
        suite.addTest(MuscleXTest("testHDFRead"))
        suite.addTest(MuscleXTest("testOpenCLDevice"))
        # suite.addTest(MuscleXTest("testGPUIntegratePyFAI")) # not working with pyinstaller
        runner = unittest.TextTestRunner()
        proc = Thread(target=runner.run, args=(suite,))
        proc.start()

        if os.path.exists(self.test_path):
            prev_data = open(self.test_path, 'r').readlines()
        else:
            prev_data = ""

        self.detail.moveCursor(QTextCursor.End)
        self.detail.setFontWeight(100)
        self.detail.insertPlainText("\nRunning detailed tests of MuscleX modules.\nThis could take a few minutes...")
        QApplication.processEvents()

        progress = 0
        test_number = 0
        while progress < 100 and proc.is_alive():
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
                    self.progressBar.setValue(int(progress))
                    QApplication.processEvents()

                    self.detail.moveCursor(QTextCursor.End)
                    self.detail.insertPlainText(f"\nFinished test {test_number} out of {NTESTS}.\n")
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
        self.detail.insertPlainText(f"\nTest results:\n{'-'*80}{test_results}{'-'*80}\nSee the log at {self.test_path} for more info.")
        QApplication.processEvents()
        proc.join()

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
        while idx < 13:
            last_test = data.split('-'*80)[-idx]
            if last_test == '\n':
                idx += 1
            else:
                break
        file.close()
        return last_test

    def get_release_results(self):
        """
        Display the last release results and versions from the test log.
        """
        if os.path.exists(self.release_path):
            file = open(self.release_path, 'r')
        else:
            return ""
        data = file.read()
        idx = 1
        while idx < 13:
            release = data.split('-'*80)[-idx]
            if release == '\n':
                idx += 1
            else:
                break
        file.close()
        return release

if __name__ == "__main__":
    LauncherForm.main()
