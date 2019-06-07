import unittest
import os
import sys
import logging
import platform
from time import gmtime, strftime

import musclex
from musclex.tests.test_utils import module_test, hdf_read_test, gpu_device_test, pyfai_gpu_integrate_test

class MuscleXTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.currdir = os.path.dirname(__file__)
        cls.inpath = os.path.join(cls.currdir, "test_images")
        cls.dipath = os.path.join(cls.inpath, "di_test_data")
        cls.hdfpath = os.path.join(cls.dipath, "test.hdf")
        cls.hdfpickle = os.path.join(cls.inpath, "hdf_record", "hdfdata_record.p")
        cls.testversion = "1.14.8" # change this to test against a different version

        system = platform.system()
        node = platform.node()
        proc = platform.processor()
        version = platform.version()
        machine = platform.machine()
        arch = platform.architecture()
        python_version = platform.python_version()
        osx_ver, osx_info, osx_machine = platform.mac_ver()
        win_rel, win_ver, win_csd, proc_type = platform.win32_ver()
        lin_name, lin_ver, lin_id = platform.linux_distribution()

        sysinfo = """\nSYSTEM INFO
System: {}
Node: {}
Processor: {}
Version: {}
Machine: {}
Architecture: {}
Python Version: {}
OSX Version: {}
OSX Info: {}
Windows Release: {}
Windows Version: {}
Windows CSD: {}
Windows OS Type: {}
Linux Distribution Name: {}
Linux Version: {}
Linux ID: {}\n
""".format(system, node, proc, version, machine, arch, python_version,
                   osx_ver, osx_info, win_rel, win_ver, win_csd, proc_type,
                   lin_name, lin_ver, lin_id)

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
            lf.write("Testing MuscleX version: {}\n".format(musclex.__version__))
            lf.write(sysinfo)
            lf.write("Package Information\n")
            try:
                import h5py
                lf.write("h5py Version: {}\n".format(h5py.__version__))
            except:
                lf.write("Unable to import h5py\n")
            try:
                import lmfit
                lf.write("lmfit Version: {}\n".format(lmfit.__version__))
            except:
                lf.write("Unable to import lmfit\n")

            lf.write("OpenCL Information\n")
            try:
                import pyopencl
                p = pyopencl.get_platforms()[0]
                opencl_version = p.version
                cpus = p.get_devices(pyopencl.device_type.CPU)
                gpus = p.get_devices(pyopencl.device_type.GPU)
                lf.write("OpenCL Version: {}\n".format(opencl_version))
                lf.write("CPUs: {}\n".format(cpus))
                lf.write("GPUs: {}\n".format(gpus))
            except:
                lf.write("OpenCL not available. Check pyopencl installation.\n")

            lf.write("\nSummary of Test Results\n")

    @classmethod
    def tearDownClass(cls):
        with open(cls.logname, 'a') as lf:
            lf.write("Ending test at {}\n".format(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
            lf.write("\n{}\n".format("-"*80))

    def testEquatorImage(self):
        """
        Runs a test of EquatorImage using the given settings.
        """
        settingsA = {
            "left_sigmac" : 1.0, "right_sigmac" : 1.0, "orientation_model" : 0,
            "nPeaks" : 2, "model" : "Gaussian", "isSkeletal" : True,
            "mask_thres" : -1.0, "90rotation" : False, "blank_mask" : False,
            "no_cache" : True
            }
        # settingsB = {
        #     "left_sigmac" : 1.0, "right_sigmac" : 1.0, "orientation_model" : 0,
        #     "nPeaks" : 5, "model" : "Voigt", "isSkeletal" : True,
        #     "mask_thres" : -1.0, "90rotation" : False, "blank_mask" : True,
        #     "no_cache" : True
        #     }

        pass_test = module_test(mode="eq",
                                settings=settingsA,
                                pickledir=os.path.join(self.currdir, "eq/tmp_verify_settingsA"),
                                inputpath=self.inpath,
                                compdir=os.path.join(self.currdir, "eq/test_pickles_settingsA"),
                                testrecord=False,
                                testversion=self.testversion,
                                 keeppickles=False)
        self.log_results(pass_test, "Equator Image")
        self.assertTrue(pass_test,"Equator Image Test for settings configuration A failed.")

    def testQuadrantFolder(self):
        settingsQF = {
            'bgsub' : 'None',
            'sigmoid' : 0.0,
            'no_cache' : True,
            'orientation_model' : 0
        }

        pass_test = module_test(
                        mode="qf",
                        settings=settingsQF,
                        pickledir=os.path.join(self.currdir, "qf/tmp_verify_settingsQF"),
                        inputpath=self.inpath,
                        compdir=os.path.join(self.currdir, "qf/test_pickles_settingsQF"),
                        testrecord=False,
                        testversion=self.testversion,
                        keeppickles=False)
        self.log_results(pass_test, "Quadrant Folder")
        self.assertTrue(pass_test,"Quadrant Folder Test for settings configuration QF failed.")

    def testDiffractionCentroids(self):
        settingsDC = {
            'orientation_model' : 0,
            '90rotation' : False,
            'no_cache' : True
        }

        pass_test = module_test(
                            mode="dc",
                            settings=settingsDC,
                            pickledir=os.path.join(self.currdir, "dc/tmp_verify_settingsDC"),
                            inputpath=self.inpath,
                            compdir=os.path.join(self.currdir, "dc/test_pickles_settingsDC"),
                            testrecord=False,
                            testversion=self.testversion,
                            keeppickles=False)
        self.log_results(pass_test, "Diffraction Centroids")
        self.assertTrue(pass_test, "Diffraction Centroids Test for settings configuration DC failed.")

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
        pass_test = module_test(
                            mode="pt",
                            settings=settingsPT,
                            pickledir=os.path.join(self.currdir, "pt/tmp_verify_settingsPT"),
                            inputpath=self.inpath,
                            compdir=os.path.join(self.currdir, "pt/test_pickles_settingsPT"),
                            testrecord=False,
                            testversion=self.testversion,
                            keeppickles=False)
        self.log_results(pass_test, "Projection Traces")
        self.assertTrue(pass_test, "Projection Traces Test for settings configuration PT has failed.")

    def testScanningDiffraction(self):
        settingsDI = {}
        pass_test = module_test(
                        mode="di",
                        settings=settingsDI,
                        pickledir=os.path.join(self.currdir, "di/tmp_verify_settingsDI"),
                        inputpath=self.inpath,
                        compdir=os.path.join(self.currdir, "di/test_pickles_settingsDI"),
                        testrecord=False,
                        testversion=self.testversion,
                        keeppickles=False)
        self.log_results(pass_test, "Scanning Diffraction")
        self.assertTrue(pass_test, "Scanning Diffraction Test for settings configuration DI has failed.")

    def testHDFRead(self):
        pass_test = hdf_read_test(self.hdfpath, self.hdfpickle)
        self.log_results(pass_test, "HDF5 Read")
        self.assertTrue(pass_test, "HDF5 read test failed. Check the h5py module for updates.")

    def testGPUIntegratePyFAI(self):
        pass_test = pyfai_gpu_integrate_test()
        self.log_results(pass_test, "pyFAI Integration")
        self.assertTrue(pass_test, "PyFAI GPU acceleration is unavailable on this machine.")

    def testOpenCLDevice(self):
        pass_test = gpu_device_test()
        self.log_results(pass_test, "OpenCL GPU Device")
        self.assertTrue(pass_test, "No GPU devices found or pyopencl is not installed.")

    ############################ Non-Test Methods ##############################

    def log_results(self, pass_test, testname):
        if pass_test:
            result = 'pass'
        else:
            result = 'fail'
        with open(self.logname, 'a') as lf:
            lf.write("{tn} Test: {r}\n".format(tn=testname, r=result))

if __name__=="__main__":
    unittest.main(verbosity=2)
