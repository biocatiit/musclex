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
            'bgsub' : 'None',
            'sigmoid' : 0.0,
            'no_cache' : True
        }
        qf_image.process(settings)
        self.assertTrue('BgSubFold' in qf_image.imgCache)

    def test_qf_bgsub_1(self):
        # Test folding with 2D Convex hull + top hat background subtraction
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif")
        settings = {
            'bgsub': '2D Convexhull',
            'sigmoid': 0.07,
            'tophat2': 20,
            'no_cache': True
        }
        qf_image.process(settings)
        self.assertTrue('BgSubFold' in qf_image.imgCache)


    def test_qf_bgsub_2(self):
        # Test folding with Circular + top hat background subtraction
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif")
        settings = {
            'bgsub': 'Circularly-symmetric',
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
        self.assertTrue('BgSubFold' in qf_image.imgCache)


    def test_qf_bgsub_3(self):
        # Test folding with top hat + top hat background subtraction
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif")
        settings = {
            'bgsub': 'White-top-hats',
            'sigmoid': 0.07,
            'tophat1': 5,
            'tophat2': 20,
            'no_cache': True
        }
        qf_image.process(settings)
        self.assertTrue('BgSubFold' in qf_image.imgCache)


    def test_qf_ignore_folds(self):
        # Test without background subtraction, but adding ignore folds
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif")
        settings = {
            'bgsub': 'None',
            'sigmoid': 0.07,
            'no_cache': True,
            'ignore_folds' : set([0,3])
        }

        qf_image.process(settings)
        self.assertTrue('BgSubFold' in qf_image.imgCache)


    def test_rolving_window(self):
        # Test with Rolving Window background subtraction
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif")

        settings = {
            'bgsub': 'Roving Window',
            'win_size_x' : 10,
            'win_size_y' : 10,
            'win_sep_x' : 10,
            'win_sep_y' : 10,
            'smooth':1,
            'tension':0,
            'cirmin': 0,
            'cirmax': 25,
            'sigmoid': 0.07,
            'tophat2': 20,
            'no_cache': True
        }
        qf_image.process(settings)
        self.assertTrue('BgSubFold' in qf_image.imgCache)

    def test_smooth_boxcar(self):
        # Test with smooth background subtraction (Box car)
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif")

        settings = {
            'bgsub': 'Smoothed-BoxCar',
            'boxcar_x' : 10,
            'boxcar_y' : 10,
            'cycles' : 5,
            'sigmoid': 0.07,
            'tophat2': 20,
            'no_cache': True
        }
        qf_image.process(settings)
        self.assertTrue('BgSubFold' in qf_image.imgCache)


    def test_smooth_gauss(self):
        # Test with smooth background subtraction (Box car)
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        qf_image = QuadrantFolder(dir_path, "p60_1_cont.tif")

        settings = {
            'bgsub': 'Smoothed-Gaussian',
            'fwhm': 10,
            'cycles': 5,
            'sigmoid': 0.07,
            'tophat2': 20,
            'no_cache': True
        }
        qf_image.process(settings)
        self.assertTrue('BgSubFold' in qf_image.imgCache)

