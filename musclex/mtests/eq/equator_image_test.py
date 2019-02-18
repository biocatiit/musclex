from musclex.modules.EquatorImage import EquatorImage
from musclex import __version__

import sys, os, pickle, glob, filecmp


def equator_image_test(pickledir="tmp_verify", inputpath="test_images",
                       compdir="test_pickles", testrecord=False,
                       testversion=__version__, keeppickles=False):
    """
    Run or prepare a test of the EquatorImage module output. In testrecord
    mode, data from the inputpath directory is analyzed and the fit results
    are stored as a pickle (.p) file. In testverify mode, data from the input
    path is pickled and compared to the recorded test pickles.

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
    compdir = os.path.abspath(compdir)
    inputpath = os.path.abspath(inputpath)
    pass_test = True # tracks whether the entire test passed
    filelist = [f for f in os.listdir(inputpath) if f.endswith(".tif")]
    if len(filelist) == 0:
        print("No images found in the input path - please specify a set of test images to input.")
        return
    failed_tests = {filename : list() for filename in filelist}

    print ("\n------ TESTING EQUATOR IMAGE MODULE ------\n")
    # Process each .tif image in the input directory
    for filename in filelist:
        pass_file = True # tracks whether or not the current file is passing
        eq = EquatorImage(inputpath, filename)
        settings = {
            "left_sigmac" : 1.0, "right_sigmac" : 1.0, "orientation_model" : None,
            "nPeaks" : 2, "model" : "Voigt", "isSkeletal" : False,
            "mask_thres" : -1.0, "90rotation" : False, "blank_mask" : False
            }
        print("\n\033[3;33m---- Processing file {f} ----\033[0;3140m\n".format(f=filename))
        eq.process(settings)

        prefix = "_record" if testrecord else "_verify"
        imgname = filename.split('.')[0]
        picklename = "_eqimg"+imgname+"v"+__version__+".p"

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
        for field in eq.info:
            # Write the current info field to a pickle file
            picklepath = os.path.join(pickledir, field+prefix+picklename)
            picklefile = open(picklepath, "wb")
            pickle.dump(eq.info[field], picklefile, pickle.HIGHEST_PROTOCOL)
            picklefile.close()

            if not testrecord:
                # If the version is present, get the pickle to test against
                plist = glob.glob(compdir+"/"+field+
                                        "_record_eqimg"+imgname+
                                        "v"+testversion+"*.p")
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
                if not pass_field:
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

    # Print the results of the test over all files
    if not testrecord:
        if pass_test:
            print("\n\033[4;32m---- EQUATOR IMAGE TEST SUCCESSFUL ----\033[0;3140m")
        else:
            print("\n\033[4;31m---- EQUATOR IMAGE TEST FAILED ----\033[0;3140m")
            print("\nThe following tests failed:")
            for test in failed_tests:
                print("Image: \033[0;35m{im}\033[0;3140m" \
                      " ---> Fields: \033[0;35m{fd}\033[0;3140m"
                      .format(im=test, fd=failed_tests[test]))
    else:
        print("\033[4;32m ---- Test files written ---- \033[0;3140m")
        return

    return pass_test

if __name__=="__main__":
    args = sys.argv
    if args[1] == 'testrecord':
        equator_image_test("test_pickles",
                           inputpath="../test_images",
                           testrecord=True)
    if args[1] == 'testverify':
        equator_image_test("tmp_verify",
                           inputpath="../test_images",
                           testrecord=False)
