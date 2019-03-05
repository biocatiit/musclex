import unittest, os
from module_test import *


class MuscleXTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.currdir = os.path.dirname(__file__)
        cls.inpath = os.path.join(cls.currdir, "test_images")
        print(cls.inpath)


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
            "mask_thres" : -1.0, "90rotation" : False, "blank_mask" : False
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

    # def testProjectionTraces(self):
    #     settingsPT = {'boxes': {'1_g': ((136, 699), (524, 550)),
    #                             '1_c': ((137, 695), (463, 486)),
    #                             '4_c': ((172, 663), (380, 407)),
    #                             '4_g': ((172, 663), (600, 629)),
    #                             'off': ((467, 546), (195, 821))},
    #                   'bgsubs': {'1_g': 0,
    #                              '1_c': 1,
    #                              '4_c': 1,
    #                              '4_g': 0,
    #                              'off': 1},
    #                   'peaks': {'1_g': [60, 117, 149],
    #                             '1_c': [59, 116, 149],
    #                             '4_c': [62, 119],
    #                             '4_g': [61, 116],
    #                             'off': [27, 54, 81, 109, 136, 163, 197, 228]},
    #                   'types': {'1_g': 'h', '1_c': 'h', '4_c': 'h', '4_g': 'h', 'off': 'v'},
    #                   'hull_ranges': {'4_c': (8, 174), '1_c': (20, 205), 'off': (10, 259)},
    #                   'bgsub' : 'None',
    #                   'sigmoid' : 0.0,
    #                   'no_cache' : True,
    #                   'orientation_model' : None
    #                   }
    #     self.assertTrue(
    #         module_test(
    #             mode="pt",
    #             settings=settingsPT,
    #             pickledir=os.path.join(self.currdir, "pt/tmp_verify_settingsPT"),
    #             inputpath=self.inpath,
    #             compdir=os.path.join(self.currdir, "pt/test_pickles_settingsPT"),
    #             testrecord=False,
    #             testversion="1.14.4",
    #             keeppickles=False
    #             ),
    #         "Projection Traces Test for settings configuration PT has failed."
    #     )

if __name__=="__main__":
    unittest.main(verbosity=2)
