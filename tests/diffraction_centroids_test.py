__author__ = 'Jiranun.J'

import unittest
from biocat_modules.DiffractionCentroids import DiffractionCentroids
import os


class MyTestCase(unittest.TestCase):

    def test_diff_cent1(self):
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        fixed_ranges = [('p1', (25, 35)), ('p2', (50, 60)), ('p3', (80, 85))]
        off_mer = {'e59': 210, 's59': 195, 's51': 225, 'x2': 105, 'x3': 38, 'x1': 35, 'e51': 240, 'x4': 101}
        difCent = DiffractionCentroids(dir_path, ["Prep24-1-1-Con.tif"], 0, fixed_ranges, off_mer)
        difCent.info['no_cache'] = True
        difCent.process()
        # self.assertEqual(23, difCent.info["rmin"])
        # self.assertEqual(0, difCent.info["rotationAngle"])
        # self.assertEqual((371, 395), difCent.info["int_area"])
        # self.assertEqual((382, 561), difCent.info["center"])
        #
        # self.assertAlmostEqual(30.3317141344, difCent.info["top_centroids"][0], 2)
        # self.assertAlmostEqual(53.2654758414, difCent.info["top_centroids"][1], 2)
        # self.assertAlmostEqual(81.044870959, difCent.info["top_centroids"][2], 2)
        #
        # self.assertAlmostEqual(30.337587979, difCent.info["bottom_centroids"][0], 2)
        # self.assertAlmostEqual(53.2294171511, difCent.info["bottom_centroids"][1], 2)
        # self.assertAlmostEqual(80.9228454672, difCent.info["bottom_centroids"][2], 2)
        #
        # peak_info = difCent.info["off_mer_peak_info"]
        #
        # self.assertAlmostEqual(200.364271896, peak_info["top_right"]["centroids"][0], 2)
        # self.assertAlmostEqual(231.568915167, peak_info["top_right"]["centroids"][1], 2)
        #
        # self.assertAlmostEqual(199.679300584, peak_info["bottom_left"]["centroids"][0], 2)
        # self.assertAlmostEqual(231.081449217, peak_info["bottom_left"]["centroids"][1], 2)
        #
        # self.assertAlmostEqual(198.646839049, peak_info["bottom_right"]["centroids"][0], 2)
        # self.assertAlmostEqual(230.278632691, peak_info["bottom_right"]["centroids"][1], 2)
        #
        # self.assertAlmostEqual(199.302271766, peak_info["top_left"]["centroids"][0], 2)
        # self.assertAlmostEqual(230.568837895, peak_info["top_left"]["centroids"][1], 2)
