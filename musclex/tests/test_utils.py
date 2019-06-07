import sys
import os
import pickle
import logging
import glob
import filecmp
import collections
import h5py
import shutil
import pyFAI
import numpy as np

from musclex.modules.EquatorImage import EquatorImage
from musclex.modules.QuadrantFolder import QuadrantFolder
from musclex.modules.DiffractionCentroids import DiffractionCentroids
from musclex.modules.ProjectionProcessor import ProjectionProcessor
from musclex.modules.ScanningDiffraction import ScanningDiffraction
from musclex.csv_manager.CP_CSVManager import CP_CSVManager
from musclex import __version__


def module_test(mode, settings, pickledir, inputpath, compdir=None,
                testrecord=False, testversion=__version__, keeppickles=False):
    """
    Run or prepare a test of a module output. In testrecord
    mode, data from the inputpath directory is analyzed and the fit results
    are stored as a pickle (.p) file. In testverify mode, data from the input
    path is pickled and compared to the recorded test pickles.

    :param mode: specifies the module to test - {eq, qf, pt, dc, di}
    :param settings: a dictionary of settings to run the module with
    :param pickledir: the path where generated pickle files are saved
    :param inputpath: the path to the test images to process
    :param compdir: the path to the pickle files to compare against
    :param testversion: verify pickles against this version of MuscleX
    :param keeppickles: set True to keep the verify pickles for debugging
    :param testrecord: if True, run in verify mode, else run in testrecord
    :return pass_test: in testverify mode - returns True if the generated pickle
                            file is equivalent to the previously generated test
                            pickle
                       in testrecord, return null
    """

    pickledir = os.path.abspath(pickledir)
    if not os.path.exists(pickledir):
        os.makedirs(pickledir)

    if not testrecord:
        compdir = os.path.abspath(compdir)
    inputpath = os.path.abspath(inputpath)

    pass_test = True # tracks whether the entire test passed
    if mode == 'di':
        inputpath = os.path.join(inputpath, "di_test_data")
    filelist = [f for f in os.listdir(inputpath) if f.endswith(".tif")]
    if len(filelist) == 0:
        print("No images found in the input path - please specify a set of test images to input.")
        return
    failed_tests = {filename : list() for filename in filelist}

    for filename in filelist:
        pass_file = True

        if mode == 'eq':
            test_object = EquatorImage(inputpath, filename)
            test_name = "EQUATOR IMAGE"
        elif mode == 'qf':
            test_object = QuadrantFolder(inputpath, filename)
            test_name = "QUADRANT FOLDER"
        elif mode == 'dc':
            test_object = DiffractionCentroids(inputpath, [filename], 0,
                                               [('peak1',(100,900))], None)
            test_name = "DIFFRACTION CENTROIDS"
        elif mode == 'pt':
            test_object = ProjectionProcessor(inputpath, filename)
            test_name = "PROJECTION TRACES"
        elif mode == 'di':
            test_object = ScanningDiffraction(inputpath, filename)
            test_name = "SCANNING DIFFRACTION"
        else:
            raise ValueError("No program mode {}".format(mode))

        print("\n\033[3;33m---- Processing file {f} ----\033[0;3140m\n".format(f=filename))
        test_object.process(settings.copy())
        print(test_object.info.keys())
        results = flatten(test_object.info)

        prefix = "_record" if testrecord else "_verify"
        imgname = filename.split('.')[0]
        picklename = "_{mode}_{name}v{version}.p".format(mode=mode,
                                                         name=imgname,
                                                         version=__version__)

        print("\033[0;33m\nProcessing complete. Writing results to {p}.\033[0;3140m".format(p=pickledir))

        if not testrecord:
            print("\033[0;33mBeginning test comparisons to recorded pickles in {cd}\033[0;3140m.".format(cd=compdir))

            # Check that the test version is present
            vlist = glob.glob(compdir+"/*v"+testversion+".p")
            if len(vlist) == 0:
                print("\033[1;31m \nTEST FAILED -- NO TEST PICKLES FOUND --")
                print("\033[0;3840mNo test pickle files corresponding to " \
                      " version {ver} found in folder {p}." \
                      "\033[0;3140mDid you run \'python equator_image_test.py testrecord\'" \
                      " from the MuscleX version you're trying to test against?\n"
                      .format(ver=testversion, p=compdir))
                pass_test = False

                return pass_test

        # Test each field in info for equivalence with the field in the test directory
        for field in results:
            # Write the current info field to a pickle file
            picklepath = os.path.join(pickledir, field+prefix+picklename)
            picklefile = open(picklepath, "wb")
            pickle.dump(results[field], picklefile, protocol=2)
            picklefile.close()
            if mode == 'di':
                cp_csvmgr = CP_CSVManager(dir_path=inputpath)
                cp_csvmgr.write_new_data(test_object)

            if not testrecord:
                # If the version is present, get the pickle to test against
                globstr = "{cd}/{fld}_record_{mode}_{im}v{v}*.p".format(cd=compdir, fld=field, mode=mode, im=imgname, v=testversion)
                plist = glob.glob(globstr)

                if len(plist) == 0:
                    print("Testing {data} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m".format(data=field))
                    print("---> \033[0;31mNo corresponding test pickle was found. Perhaps this field " \
                          "did not exist in MuscleX Version {ver}?\033[0;3840m".format(ver=testversion))
                    pass_file = False
                    pass_test = False
                    failed_tests[filename].append(field)
                    continue

                test_pickle = plist[0]
                # If the two files are identical, this yields true
                pass_field = filecmp.cmp(test_pickle, picklepath)

                # If the two files aren't identical, return some error
                # Program_version is usually contained as well, but do not compare those
                if field is not 'program_version' and not pass_field:
                    print("Testing {data} ..... \033[0;31mFAILED\033[0;3140m\033[0;3840m".format(data=field))
                    print("Compare the following files for more information:\n" \
                          "File generated for testing: {p1}\nReference file: {p2}" \
                          .format(p1 = picklepath, p2 = test_pickle))
                    keeppickles = True
                    pass_test = False
                    pass_file = False
                    failed_tests[filename].append(field)
                else:
                    print("Testing {data} ..... \033[0;32mPASSED\033[0;3140m".format(data=field))
                if not keeppickles: # Remove the verify pickles
                    os.remove(picklepath)

        # Print the results of the test for the current file
        if not testrecord:
            if pass_file:
                print("\n\033[0;32m--- Test successful for file {f}---\033[0;3140m".format(f=filename))
            else:
                print ("\n\033[0;31m--- Test failed for file {f}---\033[0;3140m".format(f=filename))

    # Remove some cache folders for the two modules that don't have no cache options
    if os.path.exists(os.path.join(inputpath, "eq_cache")):
        shutil.rmtree(os.path.join(inputpath, "eq_cache"))
    if os.path.exists(os.path.join(inputpath, "cp_cache")):
        shutil.rmtree(os.path.join(inputpath, "cp_cache"))

    # Print the results of the test over all files
    if not testrecord:
        if mode == 'di':
            print("\033[3;33mComparing Scanning Diffraction results...\033[0;3140m\n")
            summ_compare = filecmp.cmp(os.path.join(inputpath, 'cp_results', 'summary.csv'),
                            os.path.join(inputpath, 'cp_results', 'rcd_summary.csv'))
            ring_compare = filecmp.cmp(os.path.join(inputpath, 'cp_results', 'rings.csv'),
                            os.path.join(inputpath, 'cp_results', 'rcd_rings.csv'))
            pass_test = summ_compare and ring_compare
            os.remove(os.path.join(inputpath, 'cp_results', 'summary.csv'))
            os.remove(os.path.join(inputpath, 'cp_results', 'rings.csv'))
        if pass_test:
            print("\n\033[4;32m---- {} TEST SUCCESSFUL ----\033[0;3140m"
                  .format(test_name))
        else:
            print("\n\033[4;31m---- {} TEST FAILED ----\033[0;3140m"
                  .format(test_name))
            print("\nThe following tests failed:")
            for test in failed_tests:
                print("Image: \033[0;35m{im}\033[0;3140m" \
                      " ---> Fields: \033[0;35m{fd}\033[0;3140m"
                      .format(im=test, fd=failed_tests[test]))
    else:
        print("\033[4;32m ---- Test files written ---- \033[0;3140m")
        if mode == 'di':
            os.rename(os.path.join(inputpath, 'cp_results', 'summary.csv'),
                      os.path.join(inputpath, 'cp_results', 'rcd_summary.csv'))
            os.rename(os.path.join(inputpath, 'cp_results', 'rings.csv'),
                      os.path.join(inputpath, 'cp_results', 'rcd_rings.csv'))
        return

    return pass_test

def hdf_read_test(hdfpath, rcd_pickle, testrecord=False):
    hdffile = h5py.File(hdfpath)
    data = np.array(hdffile.get('data').get('BL'))

    if testrecord:
        with open(rcd_pickle, "wb") as rp:
            pickle.dump(data, rp, protocol=2)
            print("\033[3;33m\nPickling HDF5 data to {}..\033[0;3140m\n"
                  .format(rcd_pickle))
            return
    else:
        print("\033[3;33m\nVerifying that data read from {} is equivalent to previously recorded data in {}\033[0;3140m\n"
              .format(hdfpath, rcd_pickle))
        vfy_pickle = os.path.join(os.path.dirname(hdfpath), "hdfdata_verify.p")
        with open(vfy_pickle, "wb") as pf:
            pickle.dump(data, pf, protocol=2)
        pass_test = filecmp.cmp(vfy_pickle, rcd_pickle)
        if pass_test:
            print("\n\033[4;32m---- HDF5 TEST SUCCESSFUL ----\033[0;3140m")
            os.remove(vfy_pickle)
        else:
            print("\n\033[4;31m---- HDF5 TEST FAILED ----\033[0;3140m")
        return pass_test

def gpu_device_test():
    try:
        from pyFAI import opencl
        platform = opencl.pyopencl.get_platforms()[0]
        gpu_devices = platform.get_devices(device_type=opencl.pyopencl.device_type.GPU)
        if len(gpu_devices) > 0:
            print("\n\033[4;32m---- OPENCL DEVICE TEST SUCCESSFUL ----\033[0;3140m")
            print("GPU acceleration of pyFAI using OpenCL is available")
            print("GPU devices available to pyFAI and OpenCL:\n{}".format(gpu_devices))
            return True
        print("OpenCL is installed, but no GPU devices were found. GPU acceleration of pyFAI is unavailable.")
        return False
    except:
        print("\n\033[4;31m---- OPENCL DEVICE TEST FAILED ----\033[0;3140m")
        print("Check to make sure that pyopencl was installed properly or \
              proceed without GPU acceleration if other tests pass.")
        return False


def pyfai_gpu_integrate_test():
    ai = pyFAI.AzimuthalIntegrator(detector="agilent_titan")
    data = np.random.random((2048,2048))
    npt = 1000
    pass_test = True
    try:
        ai.integrate1d(data, 1000, unit="r_mm", method="csr_ocl")
    except:
        pass_test = False

    if pass_test:
        print("\n\033[4;32m---- PYFAI GPU TEST SUCCESSFUL ----\033[0;3140m")
    else:
        print("\n\033[4;31m---- PYFAI GPU TEST FAILED ----\033[0;3140m")
        print("\n\033[4;31mGPU acceleration of integration methods is not available on this machine..\033[0;3140m")
    return pass_test

# Flattens nested dictionaries
def flatten(d, parent_key='', sep='_'):
    items = []
    for k, v in d.items():
        k = str(k)
        new_key = parent_key + sep + k if parent_key else k
        if isinstance(v, collections.MutableMapping):
            items.extend(flatten(v, new_key, sep=sep).items())
        else:
            items.append((new_key, v))
    return dict(items)

if __name__=="__main__":

    inpath = os.path.abspath(os.path.join(os.path.dirname(__file__),"test_images"))
    settingsA = {
        "left_sigmac" : 1.0, "right_sigmac" : 1.0, "orientation_model" : 0,
        "nPeaks" : 2, "model" : "Gaussian", "isSkeletal" : True,
        "mask_thres" : -1.0, "90rotation" : False, "blank_mask" : False,
        "no_cache" : True
        }
    settingsB = {
        "left_sigmac" : 1.0, "right_sigmac" : 1.0, "orientation_model" : 0,
        "nPeaks" : 5, "model" : "Voigt", "isSkeletal" : True,
        "mask_thres" : -1.0, "90rotation" : False, "blank_mask" : True,
        "no_cache" : True
        }
    settingsQF = {
        'bgsub' : 'None',
        'sigmoid' : 0.0,
        'no_cache' : True,
        'orientation_model' : 0
    }
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
    settingsDC = {
        'orientation_model' : 0,
        '90rotation' : False,
        'no_cache' : True
    }
    settingsDI = {}

    args = sys.argv

    if args[1] == 'testrecord':
        module_test(mode="eq",
                    settings=settingsA,
                    pickledir=os.path.join(os.path.dirname(__file__), "eq/test_pickles_settingsA"),
                    inputpath=inpath,
                    testrecord=True)
        # module_test(mode="eq",
        #             settings=settingsB,
        #             pickledir=os.path.join(os.path.dirname(__file__), "eq/test_pickles_settingsB"),
        #             inputpath=inpath,
        #             testrecord=True)
        module_test(mode="qf",
                    settings=settingsQF,
                    pickledir=os.path.join(os.path.dirname(__file__), "qf/test_pickles_settingsQF"),
                    inputpath=inpath,
                    testrecord=True)
        module_test(mode="dc",
                    settings=settingsDC,
                    pickledir=os.path.join(os.path.dirname(__file__), "dc/test_pickles_settingsDC"),
                    inputpath=inpath,
                    testrecord=True)
        module_test(mode="pt",
                    settings=settingsPT,
                    pickledir=os.path.join(os.path.dirname(__file__), "pt/test_pickles_settingsPT"),
                    inputpath=inpath,
                    testrecord=True)
        module_test(mode="di",
                    settings=settingsDI,
                    pickledir=os.path.join(os.path.dirname(__file__), "di/test_pickles_settingsDI"),
                    inputpath=inpath,
                    testrecord=True)
        hdf_read_test(hdfpath=os.path.join(inpath, "di_test_data", "test.hdf"),
                      rcd_pickle=os.path.join(inpath, "hdf_record", "hdfdata_record.p"),
                      testrecord=True)

    if args[1] == 'testverify':
        # module_test(mode="eq",
        #             settings=settingsA,
        #             pickledir=os.path.join(os.path.dirname(__file__), "eq/tmp_verify_settingsA"),
        #             inputpath=inpath,
        #             compdir=os.path.join(os.path.dirname(__file__), "eq/test_pickles_settingsA"),
        #             testrecord=False)
        # module_test(mode="eq",
        #             settings=settingsB,
        #             pickledir=os.path.join(os.path.dirname(__file__), "eq/tmp_verify_settingsB"),
        #             inputpath=inpath,
        #             compdir=os.path.join(os.path.dirname(__file__), "eq/test_pickles_settingsB"),
        #             testrecord=False)
        # module_test(mode="qf",
        #             settings=settingsQF,
        #             pickledir=os.path.join(os.path.dirname(__file__), "pt/tmp_verify_settingsQF"),
        #             inputpath=inpath,
        #             compdir=os.path.join(os.path.dirname(__file__), "pt/test_pickles_settingsQF"),
        #             testrecord=False)
        # module_test(mode="dc",
        #             settings=settingsDC,
        #             pickledir=os.path.join(os.path.dirname(__file__), "pt/tmp_verify_settingsDC"),
        #             inputpath=inpath,
        #             compdir=os.path.join(os.path.dirname(__file__), "pt/test_pickles_settingsDC"),
        #             testrecord=False)
        # module_test(mode="pt",
        #             settings=settingsPT,
        #             pickledir=os.path.join(os.path.dirname(__file__), "pt/tmp_verify_settingsPT"),
        #             inputpath=inpath,
        #             compdir=os.path.join(os.path.dirname(__file__), "pt/test_pickles_settingsPT"),
        #             testrecord=False)
        module_test(mode="di",
                    settings=settingsDI,
                    pickledir=os.path.join(os.path.dirname(__file__), "di/tmp_verify_settingsDI"),
                    inputpath=inpath,
                    compdir=os.path.join(os.path.dirname(__file__), "di/test_pickles_settingsDI"),
                    testrecord=False)
