__author__ = 'Jiranun.J'

import unittest
from musclex.modules.QuadrantFolder import QuadrantFolder
import os


class MyTestCase(unittest.TestCase):
# class MyTestCase():

    def test_qf_image_no_bgsub(self):
        # Test folding without background subtraction
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif")
        settings = {
            'bgsub' : 0,
            'sigmoid' : 0.0,
            'no_cache' : True
        }
        qf_image.process(settings)
        self.assertAlmostEqual(16.0611, qf_image.imgCache['BgSubFold'][-50,-50], 2)

    def test_qf_bgsub_1(self):
        # Test folding with 2D Convex hull + top hat background subtraction
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif")
        settings = {
            'bgsub': 1,
            'sigmoid': 0.07,
            'tophat2': 20,
            'no_cache': True
        }
        qf_image.process(settings)
        self.assertAlmostEqual(0.40572977, qf_image.imgCache['BgSubFold'][-50, -50], 2)


    def test_qf_bgsub_2(self):
        # Test folding with Circular + top hat background subtraction
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif")
        settings = {
            'bgsub': 2,
            'ignore_thres': 0,
            'sigmoid': 0.07,
            'cirmin': 0,
            'cirmax': 25,
            'radial_bin':10,
            'smooth':1,
            'tension':0,
            'tophat2' : 20,
            'no_cache': True
        }
        qf_image.process(settings)
        self.assertAlmostEqual(2.120872, qf_image.imgCache['BgSubFold'][-50,-50], 2)


    def test_qf_bgsub_3(self):
        # Test folding with top hat + top hat background subtraction
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif")
        settings = {
            'bgsub': 3,
            'sigmoid': 0.07,
            'tophat1': 5,
            'tophat2': 20,
            'no_cache': True
        }
        qf_image.process(settings)
        self.assertAlmostEqual(0.17732, qf_image.imgCache['BgSubFold'][-50, -50], 2)


    def test_qf_ignore_folds(self):
        # Test without background subtraction, but adding ignore folds
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif")
        settings = {
            'bgsub': 0,
            'sigmoid': 0.07,
            'no_cache': True,
            'ignore_folds' : set([0,3])
        }

        qf_image.process(settings)
        self.assertAlmostEqual(16.1874, qf_image.imgCache['BgSubFold'][-50, -50], 2)

