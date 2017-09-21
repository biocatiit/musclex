__author__ = 'Jiranun.J'

import unittest
from biocat_modules.QuadrantFolder import QuadrantFolder
import os


class MyTestCase(unittest.TestCase):
# class MyTestCase():

    def test_qf_image_no_bgsub(self):
        # Test folding without background subtraction
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif", 0)
        settings = {
            'bgsub' : -1,
            'sigmoid' : 0.07,
            'no_cache' : True
        }
        qf_image.process(settings)

        # print qf_image.imgCache['BgSubFold'][-50, -50]
        # print qf_image.imgCache['BgSubFold'][-50, -1]
        # print qf_image.imgCache['BgSubFold'][-1, -50]

        self.assertAlmostEqual(16.0611, qf_image.imgCache['BgSubFold'][-50,-50], 2)
        self.assertAlmostEqual(40.3704, qf_image.imgCache['BgSubFold'][-50,-1], 2)
        self.assertAlmostEqual(165.887, qf_image.imgCache['BgSubFold'][-1,-50], 2)

    def test_qf_bgsub_0(self):
        # Test folding with Circular + top hat background subtraction
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif", 0)
        settings = {
            'bgsub': 0,
            'ignore_thres': 0,
            'sigmoid': 0.07,
            'cirmin': 5,
            'cirmax': 30,
            'bin_theta':60,
            'tophat2' : 20,
            'no_cache': True
        }
        qf_image.process(settings)
        self.assertAlmostEqual(0.359532, qf_image.imgCache['BgSubFold'][-50,-50], 2)
        self.assertAlmostEqual(13.6675, qf_image.imgCache['BgSubFold'][-50,-1], 2)
        self.assertAlmostEqual(139.184, qf_image.imgCache['BgSubFold'][-1,-50], 2)

    def test_qf_bgsub_1(self):
        # Test folding with 2D Convex hull + top hat background subtraction
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif", 0)
        settings = {
            'bgsub': 1,
            'sigmoid': 0.07,
            'tophat2': 20,
            'no_cache': True
        }
        qf_image.process(settings)
        # print qf_image.imgCache['BgSubFold'][-50, -50]
        # print qf_image.imgCache['BgSubFold'][-50, -1]
        # print qf_image.imgCache['BgSubFold'][-1, -50]
        self.assertAlmostEqual(0.410407, qf_image.imgCache['BgSubFold'][-50, -50], 2)
        self.assertAlmostEqual(9.59649, qf_image.imgCache['BgSubFold'][-50, -1], 2)
        self.assertAlmostEqual(21.149, qf_image.imgCache['BgSubFold'][-1, -50], 2)

    def test_qf_bgsub_2(self):
        # Test folding with top hat + top hat background subtraction
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif", 0)
        settings = {
            'bgsub': 2,
            'sigmoid': 0.07,
            'tophat1': 5,
            'tophat2': 20,
            'no_cache': True
        }
        qf_image.process(settings)
        # print qf_image.imgCache['BgSubFold'][-50, -50]
        # print qf_image.imgCache['BgSubFold'][-50, -1]
        # print qf_image.imgCache['BgSubFold'][-1, -50]
        self.assertAlmostEqual(0.17732, qf_image.imgCache['BgSubFold'][-50, -50], 2)
        self.assertAlmostEqual(8.80014, qf_image.imgCache['BgSubFold'][-50, -1], 2)
        self.assertAlmostEqual(92.1648, qf_image.imgCache['BgSubFold'][-1, -50], 2)


    def test_qf_ignore_folds(self):
        # Test without background subtraction, but adding ignore folds
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif", 0)
        settings = {
            'bgsub': -1,
            'sigmoid': 0.07,
            'no_cache': True
        }
        # Add ignore folds
        qf_image.info['ignore_folds'].add(0)
        qf_image.info['ignore_folds'].add(3)

        qf_image.process(settings)

        # print qf_image.imgCache['BgSubFold'][-50, -50]
        # print qf_image.imgCache['BgSubFold'][-50, -1]
        # print qf_image.imgCache['BgSubFold'][-1, -50]
        self.assertAlmostEqual(16.1874, qf_image.imgCache['BgSubFold'][-50, -50], 2)
        self.assertAlmostEqual(40.6247, qf_image.imgCache['BgSubFold'][-50, -1], 2)
        self.assertAlmostEqual(158.498, qf_image.imgCache['BgSubFold'][-1, -50], 2)
