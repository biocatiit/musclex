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

import os
import sys
import unittest
# import subprocess
import shutil
import pandas as pd
import numpy as np
from time import gmtime, strftime
from math import floor, log10
from musclex import __version__
try:
    from ..headless.EQStartWindowh import EQStartWindowh
    from ..headless.QuadrantFoldingh import QuadrantFoldingh
    from ..headless.DIImageWindowh import DIImageWindowh
    from ..headless.ProjectionTracesh import ProjectionTracesh
except: # for coverage
    from headless.EQStartWindowh import EQStartWindowh
    from headless.QuadrantFoldingh import QuadrantFoldingh
    from headless.DIImageWindowh import DIImageWindowh
    from headless.ProjectionTracesh import ProjectionTracesh

N = 4 # number of significant digits

class MuscleXGlobalTester(unittest.TestCase):
    """
    Unittest class testing musclex through headless version and comparing it to saved results created with GUI
    """
    @classmethod
    def setUpClass(cls):
        if getattr(sys, 'frozen', False):
            cls.currdir = os.path.join(os.path.dirname(sys._MEIPASS), "musclex")
            cls.run_cmd = "./musclex-main"
        else:
            cls.currdir = os.path.dirname(__file__)
            cls.run_cmd = "musclex"
        cls.inpath = os.path.join(cls.currdir, "test_images")
        cls.testversion = __version__ # change this to test against a different version
        cls.input_types = ['.adsc', '.cbf', '.edf', '.fit2d', '.mar345', '.marccd', '.pilatus', '.tif', '.hdf5', '.smv']
        cls.logname = os.path.join(cls.currdir,"test_logs", "test.log")
        if not os.path.isdir(os.path.dirname(cls.logname)):
            os.mkdir(os.path.dirname(cls.logname))
        if os.path.exists(cls.logname):
            append_write = 'a'
        else:
            append_write = 'w'

        with open(cls.logname, append_write) as lf:
            lf.write("\n{}\n".format("-"*80))
            lf.write("Beginning test at {}\n".format(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
            lf.write(f"Testing MuscleX version: {__version__}\n")
            lf.write("\nSummary of Test Results\n")

    @classmethod
    def tearDownClass(cls):
        with open(cls.logname, 'a') as lf:
            lf.write("Ending test at {}\n".format(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
            lf.write(f'\n{"-"*80}\n')

    ####### EQUATOR TEST #######
    def testHeadlessMarEquator(self):
        mar_dir = os.path.join(self.currdir, "testImages", "MARimages")
        for filename in os.listdir(mar_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(mar_dir, filename)
                EQStartWindowh(f, True, True, os.path.join(mar_dir, "eqsettings.json"))
                # subprocess.Popen([self.run_cmd, os.path.join(self.currdir, "..", "main.py"), "eq", "-h", "-i", f, "-s", os.path.join(mar_dir, "eqsettings.json"), "-d"], cwd=self.currdir).wait()

        print(f"\033[3;33m\nVerifying that generated headless Equator is equivalent to GUI Equator\033[0;3140m")
        generated_results = os.path.join(mar_dir, "eq_results", "summary2.csv")
        release_results = os.path.join(self.currdir, "testResults", "MARimages", "eq_results", "summary2.csv")
        pass_test = True
        file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(3) # Rounds up to 3 decimals to avoid computer errors
        file2 = pd.read_csv(release_results).applymap(custom_round) # .round(3)
        res = pd.merge(file1, file2)
        if len(res.index) != len(file1.index):
            pass_test = False
        if not pass_test:
            print(f"\nTesting Equator on {mar_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing Equator on {mar_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "Equator MAR Image")
        self.assertTrue(pass_test,"Equator Image Headless Test for MAR image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(mar_dir, "eq_cache")):
            shutil.rmtree(os.path.join(mar_dir, "eq_cache"))

    def testHeadlessEigerEquator(self):
        eiger_dir = os.path.join(self.currdir, "testImages", "EIGERimages")
        for filename in os.listdir(eiger_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(eiger_dir, filename)
                EQStartWindowh(f, True, True, os.path.join(eiger_dir, "eqsettings.json"))
                # subprocess.Popen([self.run_cmd, os.path.join(self.currdir, "..", "main.py"), "eq", "-h", "-i", f, "-s", os.path.join(eiger_dir, "eqsettings.json"), "-d"], cwd=self.currdir).wait()

        print(f"\033[3;33m\nVerifying that generated headless Equator is equivalent to GUI Equator\033[0;3140m")
        generated_results = os.path.join(eiger_dir, "eq_results", "summary2.csv")
        release_results = os.path.join(self.currdir, "testResults", "EIGERimages", "eq_results", "summary2.csv")
        pass_test = True
        file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(1)
        file2 = pd.read_csv(release_results).applymap(custom_round) # .round(1)
        res = pd.merge(file1, file2)
        if len(res.index) != len(file1.index):
            pass_test = False
        if not pass_test:
            print(f"\nTesting Equator on {eiger_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing Equator on {eiger_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "Equator EIGER Image")
        self.assertTrue(pass_test,"Equator Image Headless Test for EIGER image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(eiger_dir, "eq_cache")):
            shutil.rmtree(os.path.join(eiger_dir, "eq_cache"))

    def testHeadlessPilatusEquator(self):
        pilatus_dir = os.path.join(self.currdir, "testImages", "PILATUSimages")
        for filename in os.listdir(pilatus_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(pilatus_dir, filename)
                EQStartWindowh(f, True, True, os.path.join(pilatus_dir, "eqsettings.json"))
                # subprocess.Popen([self.run_cmd, os.path.join(self.currdir, "..", "main.py"), "eq", "-h", "-i", f, "-s", os.path.join(pilatus_dir, "eqsettings.json"), "-d"], cwd=self.currdir).wait()

        print(f"\033[3;33m\nVerifying that generated headless Equator is equivalent to GUI Equator\033[0;3140m")
        generated_results = os.path.join(pilatus_dir, "eq_results", "summary2.csv")
        release_results = os.path.join(self.currdir, "testResults", "PILATUSimages", "eq_results", "summary2.csv")
        pass_test = True
        file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(2)
        file2 = pd.read_csv(release_results).applymap(custom_round) # .round(1)
        res = pd.merge(file1, file2)
        if len(res.index) != len(file1.index):
            pass_test = False
        if not pass_test:
            print(f"\nTesting Equator on {pilatus_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing Equator on {pilatus_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "Equator PILATUS Image")
        self.assertTrue(pass_test,"Equator Image Headless Test for PILATUS image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(pilatus_dir, "eq_cache")):
            shutil.rmtree(os.path.join(pilatus_dir, "eq_cache"))

    ####### QUADRANT FOLDER TEST #######
    def testHeadlessMarQuadrantFolder(self):
        mar_dir = os.path.join(self.currdir, "testImages", "MARimages")
        for filename in os.listdir(mar_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(mar_dir, filename)
                QuadrantFoldingh(f, True, True, os.path.join(mar_dir, "qfsettings.json"))
                # subprocess.Popen([self.run_cmd, os.path.join(self.currdir, "..", "main.py"), "qf", "-h", "-i", f, "-s", os.path.join(mar_dir, "qfsettings.json"), "-d"], cwd=self.currdir).wait()

        print(f"\033[3;33m\nVerifying that generated headless QuadrantFolder is equivalent to GUI QuadrantFolder\033[0;3140m")
        generated_results = os.path.join(mar_dir, "qf_results", "summary.csv")
        release_results = os.path.join(self.currdir, "testResults", "MARimages", "qf_results", "summary.csv")
        pass_test = True
        file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        res = pd.merge(file1, file2)
        if len(res.index) != len(file1.index):
            pass_test = False
        if not pass_test:
            print(f"\nTesting QuadrantFolder on {mar_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing QuadrantFolder on {mar_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "QuadrantFolder MAR Image")
        self.assertTrue(pass_test,"QuadrantFolder Image Headless Test for MAR image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(mar_dir, "qf_cache")):
            shutil.rmtree(os.path.join(mar_dir, "qf_cache"))

    def testHeadlessEigerQuadrantFolder(self):
        eiger_dir = os.path.join(self.currdir, "testImages", "EIGERimages")
        for filename in os.listdir(eiger_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(eiger_dir, filename)
                QuadrantFoldingh(f, True, True, os.path.join(eiger_dir, "qfsettings.json"))
                # subprocess.Popen([self.run_cmd, os.path.join(self.currdir, "..", "main.py"), "qf", "-h", "-i", f, "-s", os.path.join(eiger_dir, "qfsettings.json"), "-d"], cwd=self.currdir).wait()

        print(f"\033[3;33m\nVerifying that generated headless QuadrantFolder is equivalent to GUI QuadrantFolder\033[0;3140m")
        generated_results = os.path.join(eiger_dir, "qf_results", "summary.csv")
        release_results = os.path.join(self.currdir, "testResults", "EIGERimages", "qf_results", "summary.csv")
        pass_test = True
        file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        res = pd.merge(file1, file2)
        if len(res.index) != len(file1.index):
            pass_test = False
        if not pass_test:
            print(f"\nTesting QuadrantFolder on {eiger_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing QuadrantFolder on {eiger_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "QuadrantFolder EIGER Image")
        self.assertTrue(pass_test,"QuadrantFolder Image Headless Test for EIGER image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(eiger_dir, "qf_cache")):
            shutil.rmtree(os.path.join(eiger_dir, "qf_cache"))

    def testHeadlessPilatusQuadrantFolder(self):
        pilatus_dir = os.path.join(self.currdir, "testImages", "PILATUSimages")
        for filename in os.listdir(pilatus_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(pilatus_dir, filename)
                QuadrantFoldingh(f, True, True, os.path.join(pilatus_dir, "qfsettings.json"))
                # subprocess.Popen([self.run_cmd, os.path.join(self.currdir, "..", "main.py"), "qf", "-h", "-i", f, "-s", os.path.join(pilatus_dir, "qfsettings.json"), "-d"], cwd=self.currdir).wait()

        print(f"\033[3;33m\nVerifying that generated headless QuadrantFolder is equivalent to GUI QuadrantFolder\033[0;3140m")
        generated_results = os.path.join(pilatus_dir, "qf_results", "summary.csv")
        release_results = os.path.join(self.currdir, "testResults", "PILATUSimages", "qf_results", "summary.csv")
        pass_test = True
        file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        res = pd.merge(file1, file2)
        if len(res.index) != len(file1.index):
            pass_test = False
        if not pass_test:
            print(f"\nTesting QuadrantFolder on {pilatus_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing QuadrantFolder on {pilatus_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "QuadrantFolder PILATUS Image")
        self.assertTrue(pass_test,"QuadrantFolder Image Headless Test for PILATUS image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(pilatus_dir, "qf_cache")):
            shutil.rmtree(os.path.join(pilatus_dir, "qf_cache"))

    ####### DIFFRACTION TEST #######
    def testHeadlessMarDiffraction(self):
        mar_dir = os.path.join(self.currdir, "testImages", "MARimages")
        for filename in os.listdir(mar_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(mar_dir, filename)
                DIImageWindowh(filename, mar_dir, True, True, os.path.join(mar_dir, "disettings.json"))
                # subprocess.Popen([self.run_cmd, os.path.join(self.currdir, "..", "main.py"), "di", "-h", "-i", f, "-s", os.path.join(mar_dir, "disettings.json"), "-d"], cwd=self.currdir).wait()

        print(f"\033[3;33m\nVerifying that generated headless Diffraction is equivalent to GUI Diffraction\033[0;3140m")
        generated_results = os.path.join(mar_dir, "di_results", "summary.csv")
        release_results = os.path.join(self.currdir, "testResults", "MARimages", "di_results", "summary.csv")
        pass_test = True
        file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        res = pd.merge(file1, file2)
        if len(res.index) != len(file1.index):
            pass_test = False
        if not pass_test:
            print(f"\nTesting Diffraction on {mar_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing Diffraction on {mar_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "Diffraction MAR Image")
        self.assertTrue(pass_test,"Diffraction Image Headless Test for MAR image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(mar_dir, "di_cache")):
            shutil.rmtree(os.path.join(mar_dir, "di_cache"))

    def testHeadlessEigerDiffraction(self):
        eiger_dir = os.path.join(self.currdir, "testImages", "EIGERimages")
        for filename in os.listdir(eiger_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(eiger_dir, filename)
                DIImageWindowh(filename, eiger_dir, True, True, os.path.join(eiger_dir, "disettings.json"))
                # subprocess.Popen([self.run_cmd, os.path.join(self.currdir, "..", "main.py"), "di", "-h", "-i", f, "-s", os.path.join(eiger_dir, "disettings.json"), "-d"], cwd=self.currdir).wait()

        print(f"\033[3;33m\nVerifying that generated headless Diffraction is equivalent to GUI Diffraction\033[0;3140m")
        generated_results = os.path.join(eiger_dir, "di_results", "summary.csv")
        release_results = os.path.join(self.currdir, "testResults", "EIGERimages", "di_results", "summary.csv")
        pass_test = True
        file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        res = pd.merge(file1, file2)
        if len(res.index) != len(file1.index):
            pass_test = False
        if not pass_test:
            print(f"\nTesting Diffraction on {eiger_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing Diffraction on {eiger_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "Diffraction EIGER Image")
        self.assertTrue(pass_test,"Diffraction Image Headless Test for EIGER image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(eiger_dir, "di_cache")):
            shutil.rmtree(os.path.join(eiger_dir, "di_cache"))

    def testHeadlessPilatusDiffraction(self):
        pilatus_dir = os.path.join(self.currdir, "testImages", "PILATUSimages")
        for filename in os.listdir(pilatus_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(pilatus_dir, filename)
                DIImageWindowh(filename, pilatus_dir, True, True, os.path.join(pilatus_dir, "disettings.json"))
                # subprocess.Popen([self.run_cmd, os.path.join(self.currdir, "..", "main.py"), "di", "-h", "-i", f, "-s", os.path.join(pilatus_dir, "disettings.json"), "-d"], cwd=self.currdir).wait()

        print(f"\033[3;33m\nVerifying that generated headless Diffraction is equivalent to GUI Diffraction\033[0;3140m")
        generated_results = os.path.join(pilatus_dir, "di_results", "summary.csv")
        release_results = os.path.join(self.currdir, "testResults", "PILATUSimages", "di_results", "summary.csv")
        pass_test = True
        file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        res = pd.merge(file1, file2)
        if len(res.index) != len(file1.index):
            pass_test = False
        if not pass_test:
            print(f"\nTesting Diffraction on {pilatus_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing Diffraction on {pilatus_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "Diffraction PILATUS Image")
        self.assertTrue(pass_test,"Diffraction Image Headless Test for PILATUS image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(pilatus_dir, "di_cache")):
            shutil.rmtree(os.path.join(pilatus_dir, "di_cache"))

    ####### PROJECTION TRACES TEST #######
    def testHeadlessMarProjectionTraces(self):
        mar_dir = os.path.join(self.currdir, "testImages", "MARimages")
        for filename in os.listdir(mar_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(mar_dir, filename)
                ProjectionTracesh(f, True, True, os.path.join(mar_dir, "ptsettings.json"))
                # subprocess.Popen([self.run_cmd, os.path.join(self.currdir, "..", "main.py"), "pt", "-h", "-i", f, "-s", os.path.join(mar_dir, "ptsettings.json"), "-d"], cwd=self.currdir).wait()

        print(f"\033[3;33m\nVerifying that generated headless ProjectionTraces is equivalent to GUI ProjectionTraces\033[0;3140m")
        generated_results = os.path.join(mar_dir, "pt_results", "summary.csv")
        release_results = os.path.join(self.currdir, "testResults", "MARimages", "pt_results", "summary.csv")
        pass_test = True
        file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        res = pd.merge(file1, file2)
        if len(res.index) != len(file1.index):
            pass_test = False
        if not pass_test:
            print(f"\nTesting ProjectionTraces on {mar_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing ProjectionTraces on {mar_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "ProjectionTraces MAR Image")
        self.assertTrue(pass_test,"ProjectionTraces Image Headless Test for MAR image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(mar_dir, "pt_cache")):
            shutil.rmtree(os.path.join(mar_dir, "pt_cache"))

    def testHeadlessEigerProjectionTraces(self):
        eiger_dir = os.path.join(self.currdir, "testImages", "EIGERimages")
        for filename in os.listdir(eiger_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(eiger_dir, filename)
                ProjectionTracesh(f, True, True, os.path.join(eiger_dir, "ptsettings.json"))
                # subprocess.Popen([self.run_cmd, os.path.join(self.currdir, "..", "main.py"), "pt", "-h", "-i", f, "-s", os.path.join(eiger_dir, "ptsettings.json"), "-d"], cwd=self.currdir).wait()

        print(f"\033[3;33m\nVerifying that generated headless ProjectionTraces is equivalent to GUI ProjectionTraces\033[0;3140m")
        generated_results = os.path.join(eiger_dir, "pt_results", "summary.csv")
        release_results = os.path.join(self.currdir, "testResults", "EIGERimages", "pt_results", "summary.csv")
        pass_test = True
        file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        res = pd.merge(file1, file2)
        if len(res.index) != len(file1.index):
            pass_test = False
        if not pass_test:
            print(f"\nTesting ProjectionTraces on {eiger_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing ProjectionTraces on {eiger_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "ProjectionTraces EIGER Image")
        self.assertTrue(pass_test,"ProjectionTraces Image Headless Test for EIGER image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(eiger_dir, "pt_cache")):
            shutil.rmtree(os.path.join(eiger_dir, "pt_cache"))

    def testHeadlessPilatusProjectionTraces(self):
        pilatus_dir = os.path.join(self.currdir, "testImages", "PILATUSimages")
        for filename in os.listdir(pilatus_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(pilatus_dir, filename)
                ProjectionTracesh(f, True, True, os.path.join(pilatus_dir, "ptsettings.json"))
                # subprocess.Popen([self.run_cmd, os.path.join(self.currdir, "..", "main.py"), "pt", "-h", "-i", f, "-s", os.path.join(pilatus_dir, "ptsettings.json"), "-d"], cwd=self.currdir).wait()

        print(f"\033[3;33m\nVerifying that generated headless ProjectionTraces is equivalent to GUI ProjectionTraces\033[0;3140m")
        generated_results = os.path.join(pilatus_dir, "pt_results", "summary.csv")
        release_results = os.path.join(self.currdir, "testResults", "PILATUSimages", "pt_results", "summary.csv")
        pass_test = True
        file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        res = pd.merge(file1, file2)
        if len(res.index) != len(file1.index):
            pass_test = False
        if not pass_test:
            print(f"\nTesting ProjectionTraces on {pilatus_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing ProjectionTraces on {pilatus_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "ProjectionTraces PILATUS Image")
        self.assertTrue(pass_test,"ProjectionTraces Image Headless Test for PILATUS image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(pilatus_dir, "pt_cache")):
            shutil.rmtree(os.path.join(pilatus_dir, "pt_cache"))

    ############################
    def log_results(self, pass_test, testname):
        """
        Save the result in the log file
        """
        if pass_test:
            result = 'pass'
        else:
            result = 'fail'
        with open(self.logname, 'a') as lf:
            lf.write(f"{testname} Test: {result}\n")

def custom_round(x):
    if isinstance(x, float) or isinstance(x, int):
        if np.isnan(x):  # Check for NaN
            return x
        else:
            if abs(x) > 0:
                return round(x, N - int(floor(log10(abs(x)))))
            else:
                return x
    else:
        return x

if __name__=="__main__":
    unittest.main(verbosity=2)
