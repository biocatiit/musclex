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
import json
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
    from ..headless.AddIntensitiesExph import AddIntensitiesExph
except: # for coverage
    from headless.EQStartWindowh import EQStartWindowh
    from headless.QuadrantFoldingh import QuadrantFoldingh
    from headless.DIImageWindowh import DIImageWindowh
    from headless.ProjectionTracesh import ProjectionTracesh
    from headless.AddIntensitiesExph import AddIntensitiesExph

N = 4 # number of significant digits
ignore_columns = []  # Columns to ignore, 1-based index
sort_key = 1  # Column to sort by, 1-based index
rtol = 1e-03
atol = 1e-06

class MuscleXGlobalTester(unittest.TestCase):
    """
    Unittest class testing musclex through headless version and comparing it to saved results created with GUI
    """
    @classmethod
    def setUpClass(cls):
        if getattr(sys, 'frozen', False):
            cls.currdir = sys._MEIPASS
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
        # pass_test = True
        # file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(3) # Rounds up to 3 decimals to avoid computer errors
        # file2 = pd.read_csv(release_results).applymap(custom_round) # .round(3)
        # res = pd.merge(file1, file2)
        # if len(res.index) != len(file1.index):
        #     pass_test = False
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=ignore_columns, sort_key=sort_key, rtol=rtol, atol=atol)
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
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=ignore_columns, sort_key=sort_key, rtol=rtol, atol=atol)
        # file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(1)
        # file2 = pd.read_csv(release_results).applymap(custom_round) # .round(1)
        # res = pd.merge(file1, file2)
        
        # if len(res.index) != len(file1.index):
        #     pass_test = False
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
        # pass_test = True
        # file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(2)
        # file2 = pd.read_csv(release_results).applymap(custom_round) # .round(1)
        # res = pd.merge(file1, file2)
        # if len(res.index) != len(file1.index):
        #     pass_test = False
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=ignore_columns, sort_key=sort_key, rtol=rtol, atol=atol)
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
    def _compareToGuiBaselineIfPresent(self, dataset_dir, generated_results, label):
        """
        Optional secondary check: if <dataset_dir>/qf_results_gui/summary.csv
        exists, compare the freshly-generated headless summary.csv against
        it. The qf_results_gui CSV is meant to be dropped in by the user
        after a manual GUI run; this lets us catch headless<->GUI drift
        without committing the GUI CSV as the authoritative baseline.

        - No-op (silent) when qf_results_gui/summary.csv is absent.
        - assertTrue-fails when present but mismatched, so the regression
          surfaces in the unittest report. Failure here is independent of
          the upstream qf_results/summary.csv baseline comparison.

        ignore_columns / sort_key / rtol / atol mirror the qf_results
        comparison to keep both checks apples-to-apples.
        """
        gui_results = os.path.join(dataset_dir, "qf_results_gui", "summary.csv")
        if not os.path.exists(gui_results):
            return

        print(f"\033[3;33m\nVerifying that headless {label} matches GUI summary at "
              f"{gui_results}\033[0;3140m")
        pass_test = compare_csv_files(
            generated_results, gui_results,
            ignore_columns=[5], sort_key=sort_key, rtol=rtol, atol=atol,
        )
        if pass_test:
            print(f"Testing {label} headless-vs-GUI on {dataset_dir} ..... "
                  f"\033[0;32mPASSED\033[0;3140m")
        else:
            print(f"\nTesting {label} headless-vs-GUI on {dataset_dir} ..... "
                  f"\033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nGUI reference file: {p2}\n" \
                    .format(p1=generated_results, p2=gui_results))
        self.log_results(pass_test, f"{label} headless-vs-GUI")
        self.assertTrue(
            pass_test,
            f"{label} headless summary does not match GUI summary at {gui_results}.",
        )

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
        # pass_test = True
        # file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        # file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        # res = pd.merge(file1, file2)
        # if len(res.index) != len(file1.index):
        #     pass_test = False
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=[5], sort_key=sort_key, rtol=rtol, atol=atol)
        if not pass_test:
            print(f"\nTesting QuadrantFolder on {mar_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing QuadrantFolder on {mar_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "QuadrantFolder MAR Image")
        self._compareToGuiBaselineIfPresent(mar_dir, generated_results, "QuadrantFolder MAR")
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
        # pass_test = True
        # file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        # file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        # res = pd.merge(file1, file2)
        # if len(res.index) != len(file1.index):
        #     pass_test = False
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=[5], sort_key=sort_key, rtol=rtol, atol=atol)
        if not pass_test:
            print(f"\nTesting QuadrantFolder on {eiger_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing QuadrantFolder on {eiger_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "QuadrantFolder EIGER Image")
        self._compareToGuiBaselineIfPresent(eiger_dir, generated_results, "QuadrantFolder EIGER")
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
        # pass_test = True
        # file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        # file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        # res = pd.merge(file1, file2)
        # if len(res.index) != len(file1.index):
        #     pass_test = False
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=[5], sort_key=sort_key, rtol=rtol, atol=atol)
        if not pass_test:
            print(f"\nTesting QuadrantFolder on {pilatus_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing QuadrantFolder on {pilatus_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "QuadrantFolder PILATUS Image")
        self._compareToGuiBaselineIfPresent(pilatus_dir, generated_results, "QuadrantFolder PILATUS")
        self.assertTrue(pass_test,"QuadrantFolder Image Headless Test for PILATUS image failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(pilatus_dir, "qf_cache")):
            shutil.rmtree(os.path.join(pilatus_dir, "qf_cache"))


    ####### QF SETTINGS BINDINGS SCHEMA TEST #######
    def testQFSettingsBindingsSchema(self):
        """
        Every key persisted in a baseline qfsettings.json must be
        classifiable by the GUI load-path binding tables. An 'unknown'
        result means either:
          - getFlags()/saveSettings() added a key without adding a
            corresponding load binding, OR
          - the JSON contains an obsolete key that should be removed
            at the source (so headless and GUI stay in sync).
        Either way, this test fails fast instead of letting Load
        Settings silently drop fields.
        """
        try:
            from ..utils.qf_settings_bindings import classify_qf_setting_key
        except ImportError:  # for coverage / packaging
            from utils.qf_settings_bindings import classify_qf_setting_key

        datasets = ("MARimages", "EIGERimages", "PILATUSimages")
        all_problems = {}
        for ds in datasets:
            path = os.path.join(self.currdir, "testImages", ds, "qfsettings.json")
            if not os.path.isfile(path):
                continue
            with open(path, 'r') as f:
                settings = json.load(f)
            unknown = sorted(
                k for k in settings
                if classify_qf_setting_key(k) == 'unknown'
            )
            if unknown:
                all_problems[ds] = unknown

        pass_test = not all_problems
        if pass_test:
            print("\nTesting QF Settings bindings schema ..... "
                  "\033[0;32mPASSED\033[0;3140m")
        else:
            print(
                "\nTesting QF Settings bindings schema ..... "
                f"\033[0;31mFAILED\033[0;3140m\nUnknown keys per dataset: "
                f"{all_problems}"
            )
        self.log_results(pass_test, "QF Settings Bindings Schema")
        self.assertTrue(
            pass_test,
            f"qfsettings.json contains keys not handled by loadSettings(): "
            f"{all_problems}",
        )


    ####### EQ SETTINGS BINDINGS SCHEMA TEST #######
    def testEQSettingsBindingsSchema(self):
        """
        Every key persisted in a baseline eqsettings.json must be
        classifiable by EquatorWindow.loadSettings()'s binding tables.
        An 'unknown' result means either:
          - getSettings() / getFittingSettings() added a key without
            adding a corresponding load binding, OR
          - the JSON contains an obsolete key that should be removed
            at the source (so headless and GUI stay in sync).
        Either way, this test fails fast instead of letting Load
        Settings silently drop fields.
        """
        try:
            from ..utils.eq_settings_bindings import classify_eq_setting_key
        except ImportError:  # for coverage / packaging
            from utils.eq_settings_bindings import classify_eq_setting_key

        datasets = ("MARimages", "EIGERimages", "PILATUSimages")
        all_problems = {}
        for ds in datasets:
            path = os.path.join(self.currdir, "testImages", ds, "eqsettings.json")
            if not os.path.isfile(path):
                continue
            with open(path, 'r') as f:
                settings = json.load(f)
            unknown = sorted(
                k for k in settings
                if classify_eq_setting_key(k) == 'unknown'
            )
            if unknown:
                all_problems[ds] = unknown

        pass_test = not all_problems
        if pass_test:
            print("\nTesting EQ Settings bindings schema ..... "
                  "\033[0;32mPASSED\033[0;3140m")
        else:
            print(
                "\nTesting EQ Settings bindings schema ..... "
                f"\033[0;31mFAILED\033[0;3140m\nUnknown keys per dataset: "
                f"{all_problems}"
            )
        self.log_results(pass_test, "EQ Settings Bindings Schema")
        self.assertTrue(
            pass_test,
            f"eqsettings.json contains keys not handled by loadSettings(): "
            f"{all_problems}",
        )


    ####### QF _apply_existing_or_default_bg REGRESSION #######
    def testApplyExistingOrDefaultBgRespectsUserChoice(self):
        """
        Regression for the "Load Settings then reprocess silently
        reverts to bgsub='None'" bug.

        Reproduced via GUI: open image with no usable bg config ->
        first run cached info['bgsub']='None' and
        info['result_bg'] = {'method': 'None', 'final_params': {}, ...}
        in qf_cache. User then 'Load Settings...' (sets widget to
        'Circularly-symmetric') and re-process. updateInfo writes the
        new bgsub to info, but result_bg is still the stale pickle.
        searchBackground finds no manual / auto / optimization match
        and falls back to _apply_existing_or_default_bg, which used
        to treat the literal string 'None' from the prior pickle as
        a valid "existing" method to reuse -- silently overwriting
        info['bgsub'] back to 'None' and corrupting the new pickle.

        This test exercises the helper directly with the two
        scenarios it has to distinguish:

          1. Caller picked a real method this run, prior pickle's
             result_bg.method is the literal 'None'. The caller's
             choice must win and result_bg must be rebuilt from
             current info.
          2. Caller did NOT pick a method (bgsub='None'/None) but a
             usable prior configuration is on file. The sticky
             reuse path must still kick in.
        """
        try:
            from ..modules.QuadrantFolder import QuadrantFolder
        except ImportError:
            from modules.QuadrantFolder import QuadrantFolder

        problems = []

        # Scenario 1: user chose 'Circularly-symmetric', stale pickle
        # holds the literal-string 'None' method with empty params.
        qf1 = QuadrantFolder.__new__(QuadrantFolder)
        qf1.info = {
            'bgsub': 'Circularly-symmetric',
            'cirmin': 0.0, 'cirmax': 25.0,
            'radial_bin': 10, 'smooth': 0.1, 'tension': 1.0,
            'result_bg': {
                'method': 'None',
                'final_params': {},
                'selected_configuration_name': '-',
                'optimized': False,
            },
        }
        qf1._apply_existing_or_default_bg()

        if qf1.info['bgsub'] != 'Circularly-symmetric':
            problems.append(
                f"Scenario 1: bgsub was silently reverted to "
                f"{qf1.info['bgsub']!r} (expected 'Circularly-symmetric')"
            )
        if qf1.info['result_bg']['method'] != 'Circularly-symmetric':
            problems.append(
                f"Scenario 1: result_bg.method = "
                f"{qf1.info['result_bg']['method']!r} (expected "
                f"'Circularly-symmetric')"
            )
        expected_params = {
            'smooth': 0.1, 'tension': 1.0, 'radial_bin': 10,
            'cirmin': 0.0, 'cirmax': 25.0,
        }
        if qf1.info['result_bg']['final_params'] != expected_params:
            problems.append(
                f"Scenario 1: result_bg.final_params not rebuilt from "
                f"current info: got {qf1.info['result_bg']['final_params']!r} "
                f"(expected {expected_params!r})"
            )

        # Scenario 2: no method this run, prior usable result on file
        # -> sticky reuse should still work (no regression of legacy
        # behavior).
        qf2 = QuadrantFolder.__new__(QuadrantFolder)
        prior_params = {
            'smooth': 0.05, 'tension': 1.0, 'radial_bin': 10,
            'cirmin': 0, 'cirmax': 25,
        }
        qf2.info = {
            'bgsub': 'None',
            'result_bg': {
                'method': 'Circularly-symmetric',
                'final_params': dict(prior_params),
                'selected_configuration_name': '-',
                'optimized': False,
            },
        }
        qf2._apply_existing_or_default_bg()

        if qf2.info['bgsub'] != 'Circularly-symmetric':
            problems.append(
                f"Scenario 2: sticky reuse did not fire; bgsub = "
                f"{qf2.info['bgsub']!r} (expected 'Circularly-symmetric')"
            )
        if qf2.info.get('smooth') != 0.05:
            problems.append(
                f"Scenario 2: prior final_params not propagated back "
                f"to top-level info (smooth = {qf2.info.get('smooth')!r}, "
                f"expected 0.05)"
            )

        # Scenario 3: stale literal-string 'None' AND no method this
        # run -> no usable prior, no user choice. Should leave
        # everything coherent at 'None' (no crash, no resurrection of
        # the bogus stale state).
        qf3 = QuadrantFolder.__new__(QuadrantFolder)
        qf3.info = {
            'bgsub': 'None',
            'result_bg': {
                'method': 'None',
                'final_params': {},
                'selected_configuration_name': '-',
                'optimized': False,
            },
        }
        qf3._apply_existing_or_default_bg()

        if qf3.info['bgsub'] != 'None':
            problems.append(
                f"Scenario 3: bgsub mutated unexpectedly to "
                f"{qf3.info['bgsub']!r}"
            )
        if qf3.info['result_bg']['method'] != 'None':
            problems.append(
                f"Scenario 3: result_bg.method mutated to "
                f"{qf3.info['result_bg']['method']!r}"
            )

        pass_test = not problems
        if pass_test:
            print(
                "\nTesting QF _apply_existing_or_default_bg ..... "
                "\033[0;32mPASSED\033[0;3140m"
            )
        else:
            print(
                "\nTesting QF _apply_existing_or_default_bg ..... "
                f"\033[0;31mFAILED\033[0;3140m\n  - "
                + "\n  - ".join(problems)
            )
        self.log_results(pass_test, "QF _apply_existing_or_default_bg")
        self.assertTrue(
            pass_test,
            "_apply_existing_or_default_bg regressed: "
            + "; ".join(problems),
        )


    ####### QF FINGERPRINT WIDGET-TRUNCATION REGRESSION #######
    def testQFFingerprintIgnoresWidgetTruncation(self):
        """
        Regression for the "loss drifts by ~1e-4 across image switches"
        bug.

        Symptom: with bgsub=Circularly-symmetric and the same image,
        bgSum/symmetry are identical across repeated GUI image switches
        but loss varies in the 4th-5th decimal. Root cause is a
        round-trip precision loss on info fields that are
        *computed-on-demand* by the pipeline (evaluation_baseline,
        synthetic_amplitude/sigma_x/sigma_y, m1, layer_line_width) and
        then read back through a QDoubleSpinBox(decimals=2)/QSpinBox in
        getFlags(). The truncated widget value overwrites the
        high-precision pickle value during updateInfo(), so
        computeFingerprint() hashes a different number than the
        fingerprint stored in the pickle -> fast path is rejected ->
        slow path re-runs evaluateResult() with the truncated baseline
        -> loss differs by ~1e-4. Fix: add these keys to
        _NON_FINGERPRINT_KEYS so the widget round-trip can no longer
        invalidate the fast path.

        This test exercises the fingerprint slice computeFingerprint()
        actually hashes, with no Qt dependency.
        """
        import json, hashlib
        try:
            from ..modules.QuadrantFolder import QuadrantFolder
        except ImportError:
            from modules.QuadrantFolder import QuadrantFolder

        non_fp = set(QuadrantFolder._NON_FINGERPRINT_KEYS)
        non_img = set(QuadrantFolder._IMAGE_ARRAY_KEYS)

        def normalize(v):
            if isinstance(v, dict):
                return {k: normalize(v[k]) for k in sorted(v.keys(), key=str)}
            if isinstance(v, set):
                return sorted([normalize(x) for x in v], key=str)
            if isinstance(v, (list, tuple)):
                return [normalize(x) for x in v]
            return v

        def params_hash(info):
            params = {k: normalize(v) for k, v in info.items()
                      if k not in non_fp and k not in non_img}
            blob = json.dumps(params, sort_keys=True, default=str).encode()
            return hashlib.sha256(blob).hexdigest()

        base = {
            'bgsub': 'Circularly-symmetric',
            'cirmin': 0.0, 'cirmax': 25.0,
            'rmin': 25, 'rmax': 1533,
            'fixed_rmin': 25, 'fixed_rmax': 1533,
            'optimize': False, 'bg_options': 0, 'downsample': 2,
        }
        # Values the pipeline writes back to info (full FP precision).
        high_prec = dict(base, **{
            'evaluation_baseline': 11234.567891234,   # decimals=2 widget
            'synthetic_amplitude': 543.7,             # decimals=0 widget
            'synthetic_sigma_x': 4.3137081,           # decimals=2 widget
            'synthetic_sigma_y': 8.6274163,           # decimals=2 widget
            'm1': 100,                                # int spinbox
            'layer_line_width': 5,                    # int spinbox
        })
        # The same info after a getFlags() round-trip through Qt spinboxes.
        widget_trunc = dict(base, **{
            'evaluation_baseline': 11234.57,
            'synthetic_amplitude': 544.0,
            'synthetic_sigma_x': 4.31,
            'synthetic_sigma_y': 8.63,
            'm1': 100,
            'layer_line_width': 5,
        })
        # A genuine parameter change -- fingerprint must still reject this.
        real_change = dict(high_prec, cirmin=2.0)

        problems = []
        if params_hash(high_prec) != params_hash(widget_trunc):
            problems.append(
                "fingerprint still drifts when widget truncation changes "
                "computed-default fields; expected the round-trip to be a "
                "no-op"
            )
        if params_hash(high_prec) == params_hash(real_change):
            problems.append(
                "fingerprint no longer detects a real parameter change "
                "(cirmin 0.0 -> 2.0); _NON_FINGERPRINT_KEYS is overreaching"
            )
        # Belt-and-suspenders: every key we documented as widget-truncated
        # must in fact be excluded from the fingerprint.
        for k in QuadrantFolder._WIDGET_TRUNCATED_DEFAULT_KEYS:
            if k not in non_fp:
                problems.append(
                    f"_WIDGET_TRUNCATED_DEFAULT_KEYS lists {k!r} but it is "
                    f"not in _NON_FINGERPRINT_KEYS"
                )

        pass_test = not problems
        if pass_test:
            print(
                "\nTesting QF fingerprint widget-truncation stability ..... "
                "\033[0;32mPASSED\033[0;3140m"
            )
        else:
            print(
                "\nTesting QF fingerprint widget-truncation stability ..... "
                f"\033[0;31mFAILED\033[0;3140m\n  - "
                + "\n  - ".join(problems)
            )
        self.log_results(pass_test, "QF fingerprint widget-truncation stability")
        self.assertTrue(
            pass_test,
            "QF fingerprint widget-truncation regression: "
            + "; ".join(problems),
        )


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
        # pass_test = True
        # file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        # file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        # res = pd.merge(file1, file2)
        # if len(res.index) != len(file1.index):
        #     pass_test = False
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=ignore_columns, sort_key=sort_key, rtol=rtol, atol=atol)
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
        # pass_test = True
        # file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        # file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        # res = pd.merge(file1, file2)
        # if len(res.index) != len(file1.index):
        #     pass_test = False
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=ignore_columns, sort_key=sort_key, rtol=rtol, atol=atol)
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
        # pass_test = True
        # file1 = pd.read_csv(generated_results).applymap(custom_round) # .round(4)
        # file2 = pd.read_csv(release_results).applymap(custom_round) # .round(4)
        # res = pd.merge(file1, file2)
        # if len(res.index) != len(file1.index):
        #     pass_test = False
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=ignore_columns, sort_key=sort_key, rtol=rtol, atol=atol)
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
    def testHeadlessMarPTConvexHullVertical(self):
        """Test ProjectionTraces with MAR_PT_Convex_Hull_Vertical (new format with embedded box config)"""
        pt_dir = os.path.join(self.currdir, "testImages", "MAR_PT_Convex_Hull_Vertical")
        for filename in os.listdir(pt_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(pt_dir, filename)
                ProjectionTracesh(f, True, True, os.path.join(pt_dir, "ptsettings.json"))

        print(f"\033[3;33m\nVerifying that generated headless ProjectionTraces is equivalent to GUI ProjectionTraces\033[0;3140m")
        generated_results = os.path.join(pt_dir, "pt_results", "summary.csv")
        release_results = os.path.join(self.currdir, "testResults", "MAR_PT_Convex_Hull_Vertical", "summary.csv")
        
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=ignore_columns, sort_key=sort_key, rtol=1, atol=atol)
        if not pass_test:
            print(f"\nTesting ProjectionTraces on {pt_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing ProjectionTraces on {pt_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "ProjectionTraces MAR_PT_Convex_Hull_Vertical")
        self.assertTrue(pass_test,"ProjectionTraces Image Headless Test for MAR_PT_Convex_Hull_Vertical failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(pt_dir, "pt_cache")):
            shutil.rmtree(os.path.join(pt_dir, "pt_cache"))

    def testHeadlessEigerPTConvexHullVertical(self):
        """Test ProjectionTraces with EIGER_PT_Convex_Hull_Vertical (new format with embedded box config)"""
        pt_dir = os.path.join(self.currdir, "testImages", "EIGER_PT_Convex_Hull_Vertical")
        for filename in os.listdir(pt_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(pt_dir, filename)
                ProjectionTracesh(f, True, True, os.path.join(pt_dir, "ptsettings.json"))

        print(f"\033[3;33m\nVerifying that generated headless ProjectionTraces is equivalent to GUI ProjectionTraces\033[0;3140m")
        generated_results = os.path.join(pt_dir, "pt_results", "summary.csv")
        release_results = os.path.join(self.currdir, "testResults", "EIGER_PT_Convex_Hull_Vertical", "summary.csv")
        
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=ignore_columns, sort_key=sort_key, rtol=1, atol=atol)
        if not pass_test:
            print(f"\nTesting ProjectionTraces on {pt_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing ProjectionTraces on {pt_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "ProjectionTraces EIGER_PT_Convex_Hull_Vertical")
        self.assertTrue(pass_test,"ProjectionTraces Image Headless Test for EIGER_PT_Convex_Hull_Vertical failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(pt_dir, "pt_cache")):
            shutil.rmtree(os.path.join(pt_dir, "pt_cache"))

    def testHeadlessPTFittingGaussiansHorizontal(self):
        """Test ProjectionTraces with PT_FittingGaussians_Horizontal (Gaussian fitting, horizontal box, quadrant folded)"""
        pt_dir = os.path.join(self.currdir, "testImages", "PT_FittingGaussians_Horizontal")
        for filename in os.listdir(pt_dir):
            _, ext = os.path.splitext(str(filename))
            if ext in self.input_types:
                f = os.path.join(pt_dir, filename)
                ProjectionTracesh(f, True, True, os.path.join(pt_dir, "ptsettings.json"))

        print(f"\033[3;33m\nVerifying that generated headless ProjectionTraces is equivalent to GUI ProjectionTraces\033[0;3140m")
        generated_results = os.path.join(pt_dir, "pt_results", "summary.csv")
        release_results = os.path.join(self.currdir, "testResults", "PT_FittingGaussians_Horizontal", "summary.csv")
        
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=ignore_columns, sort_key=sort_key, rtol=1, atol=atol)
        if not pass_test:
            print(f"\nTesting ProjectionTraces on {pt_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing ProjectionTraces on {pt_dir} ..... \033[0;32mPASSED\033[0;3140m")
        self.log_results(pass_test, "ProjectionTraces PT_FittingGaussians_Horizontal")
        self.assertTrue(pass_test,"ProjectionTraces Image Headless Test for PT_FittingGaussians_Horizontal failed.")

        # Remove cache folders
        if os.path.exists(os.path.join(pt_dir, "pt_cache")):
            shutil.rmtree(os.path.join(pt_dir, "pt_cache"))
            
    def testHeadlessAISE(self):
        aise_dir = os.path.join(self.currdir, "test_images")
        aise_settings = os.path.join(self.currdir, "test_images", "aismesettings.json")
        
        AddIntensitiesExph(aise_dir, aise_settings, 'folder', 'aise')
        
        print(f"\033[3;33m\nVerifying that generated headless AISE is equivalent to GUI AISE\033[0;3140m")
        generated_results = os.path.join(aise_dir, "aise_results", "intensities.csv")
        release_results = os.path.join(self.currdir, "testResults", "AISEimages", "intensities.csv")
        
        pass_test = True
        
        pass_test = compare_csv_files(generated_results, release_results, ignore_columns=[2], sort_key=sort_key, rtol=1, atol=1e-3)
        
        if not pass_test:
            print(f"\nTesting AISEHeadless on {aise_dir} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m")
            print("Compare the following files for more information:\n" \
                    "File generated for testing: {p1}\nReference file: {p2}\n" \
                    .format(p1 = generated_results, p2 = release_results))
        else:
            print(f"Testing AISEHeadless on {aise_dir} ..... \033[0;32mPASSED\033[0;3140m")
            
        self.log_results(pass_test, "AISE Results")
        self.assertTrue(pass_test,"AISE Image Headless Test for images failed.")
        
        results_dir = os.path.join(aise_dir, "aise_results")
        if os.path.exists(results_dir):
            shutil.rmtree(results_dir)

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
    
def compare_csv_files(file1, file2, ignore_columns=None, sort_key=None, rtol=1e-03, atol=1e-05):
    # Read the CSV files into DataFrames
    df1 = pd.read_csv(file1)
    df2 = pd.read_csv(file2)

    # Ensure both DataFrames have the same shape
    if df1.shape != df2.shape:
        return False

    # Ensure both DataFrames have the same columns
    if list(df1.columns) != list(df2.columns):
        return False

    # Adjust for ignore_columns starting from 1
    if ignore_columns:
        ignore_columns = [col - 1 for col in ignore_columns]

    # Sort by the specified sort key if provided (convert 1-based to 0-based index)
    if sort_key:
        sort_key -= 1
        if sort_key < 0 or sort_key >= df1.shape[1]:
            raise ValueError("sort_key is out of range.")
        df1 = df1.sort_values(by=df1.columns[sort_key]).reset_index(drop=True)
        df2 = df2.sort_values(by=df2.columns[sort_key]).reset_index(drop=True)

    # Compare each column
    for idx, column in enumerate(df1.columns):
        if ignore_columns and idx in ignore_columns:
            continue

        if column not in df2.columns:
            print("column not in df2", column)
            return False

        # Check if the column is numeric
        if pd.api.types.is_numeric_dtype(df1[column]) and pd.api.types.is_numeric_dtype(df2[column]):
            # Handle NaN values by treating them as equal
            if not np.allclose(df1[column].fillna(0).values, df2[column].fillna(0).values, rtol=rtol, atol=atol):
                print("Not Equal: ", column)
                print("df1:", df1[column].fillna(0).values)
                print("df2:", df2[column].fillna(0).values)
                return False
        else:
            # Handle non-numeric columns, treating NaNs as equal
            df1[column] = df1[column].astype(str).fillna('')
            df2[column] = df2[column].astype(str).fillna('')
            if not df1[column].equals(df2[column]):
                print("notequal", column)
                return False

    return True

if __name__=="__main__":
    unittest.main(verbosity=2)
