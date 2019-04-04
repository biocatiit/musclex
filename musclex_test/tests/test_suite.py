import unittest
import os
import h5py
import numpy as np
from module_test import *


class MuscleXTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.currdir = os.path.dirname(__file__)
        cls.inpath = os.path.join(cls.currdir, "test_images")
        cls.dipath = os.path.join(cls.inpath, "Mn23-1_30umThick_20umSteps_1s")
        cls.hdfpath = os.path.join(cls.dipath, "MnC23-1_30umThick_20umStep_SVZ_1s_diffScan_0001.hdf")
        cls.hdfpickle = os.path.join(cls.inpath, "hdf_record", "hdfdata_record.p")

    def testEquatorImage(self):
        """
        Runs a test of EquatorImage using the given settings.
        """
        settingsA = {
            "left_sigmac" : 1.0, "right_sigmac" : 1.0, "orientation_model" : 0,
            "nPeaks" : 2, "model" : "Gaussian", "isSkeletal" : True,
            "mask_thres" : -1.0, "90rotation" : False, "blank_mask" : False
            }
        settingsB = {
            "left_sigmac" : 1.0, "right_sigmac" : 1.0, "orientation_model" : 0,
            "nPeaks" : 5, "model" : "Voigt", "isSkeletal" : True,
            "mask_thres" : -1.0, "90rotation" : False, "blank_mask" : True
            }

        self.assertTrue(
            module_test(
                mode="eq",
                settings=settingsA,
                pickledir=os.path.join(self.currdir, "eq/tmp_verify_settingsA"),
                inputpath=self.inpath,
                compdir=os.path.join(self.currdir, "eq/test_pickles_settingsA"),
                testrecord=False,
                testversion="1.14.4",
                keeppickles=False
                ),
            "Equator Image Test for settings configuration A failed."
            )
        self.assertTrue(
            module_test(
                mode="eq",
                settings=settingsB,
                pickledir=os.path.join(self.currdir, "eq/tmp_verify_settingsB"),
                inputpath=self.inpath,
                compdir=os.path.join(self.currdir, "eq/test_pickles_settingsB"),
                testrecord=False,
                testversion="1.14.4",
                keeppickles=False
                ),
            "Equator Image Test for settings configuration B failed."
            )

    def testQuadrantFolder(self):
        settingsQF = {
            'bgsub' : 'None',
            'sigmoid' : 0.0,
            'no_cache' : True,
            'orientation_model' : 0
        }

        self.assertTrue(
            module_test(
                mode="qf",
                settings=settingsQF,
                pickledir=os.path.join(self.currdir, "qf/tmp_verify_settingsQF"),
                inputpath=self.inpath,
                compdir=os.path.join(self.currdir, "qf/test_pickles_settingsQF"),
                testrecord=False,
                testversion="1.14.4",
                keeppickles=False
                ),
            "Quadrant Folder Test for settings configuration QF failed."
        )

    def testDiffractionCentroids(self):
        settingsDC = {
            'orientation_model' : 0,
            '90rotation' : False,
            'no_cache' : True
        }

        self.assertTrue(
            module_test(
                mode="dc",
                settings=settingsDC,
                pickledir=os.path.join(self.currdir, "dc/tmp_verify_settingsDC"),
                inputpath=self.inpath,
                compdir=os.path.join(self.currdir, "dc/test_pickles_settingsDC"),
                testrecord=False,
                testversion="1.14.4",
                keeppickles=False
            ),
            "Diffraction Centroids Test for settings configuration DC failed."
        )

    def testProjectionTraces(self):
        settingsPT = {
            'boxes' : {'box1' : ((200, 800),(500, 600))},
            'bgsubs' : {'box1' : 0},
            'types' : {'box1' : 'h'},
            'peaks' : {'box1' : [100]},
            'bgsub' : 'None',
            'sigmoid' : 0.0,
            'no_cache' : True,
            'orientation_model' : 0
        }
        self.assertTrue(
            module_test(
                mode="pt",
                settings=settingsPT,
                pickledir=os.path.join(self.currdir, "pt/tmp_verify_settingsPT"),
                inputpath=self.inpath,
                compdir=os.path.join(self.currdir, "pt/test_pickles_settingsPT"),
                testrecord=False,
                testversion="1.14.4",
                keeppickles=False
                ),
            "Projection Traces Test for settings configuration PT has failed."
        )

    def testScanningDiffraction(self):
        settingsDI = {}
        self.assertTrue(
            module_test(
                mode="di",
                settings=settingsDI,
                pickledir=os.path.join(self.currdir, "di/tmp_verify_settingsDI"),
                inputpath=self.inpath,
                compdir=os.path.join(self.currdir, "di/test_pickles_settingsDI"),
                testrecord=False,
                testversion="1.14.4",
                keeppickles=False
                ),
            "Scanning Diffraction Test for settings configuration DI has failed."
        )

    def testHDFRead(self):
        self.assertTrue(hdf_read_test(self.hdfpath, self.hdfpickle),
                        "HDF5 read test failed. Check the h5py module for updates.")

if __name__=="__main__":
    unittest.main(verbosity=2)
