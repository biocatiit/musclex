__author__ = 'Jiranun.J'

import unittest
from musclex.utils.image_processor import *
import os
import fabio

class Image_proc_testcase(unittest.TestCase):
# class MyTestCase():

    def test_distance(self):
        # Test distance calculation
        self.assertEqual(distance([0., 0.], [50, 50]), distance((0., 0.), (50, 50)))
        self.assertAlmostEqual(70.7106781187, distance((0., 0.), (50, 50)), 2)
        self.assertAlmostEqual(70.7106781187, distance([0., 0.], [50, 50]), 2)
        self.assertAlmostEqual(0, distance([0., 0.], [0, 0]), 2)
        self.assertAlmostEqual(1, distance([0., 0.], (0,1)), 2)

    def test_convert(self):
        file = os.path.split(os.path.realpath(__file__))[0] + "/test_images/2013.tif"
        img = fabio.open(file).data
        imb16bit = get16bitImage(img)
        imb8bit = get8bitImage(img)
        imb8bit2 = get8bitImage(img, 0, 15)
        rgb_img = getBGR(imb8bit)

        self.assertEqual(65535, imb16bit.max())
        self.assertEqual(0, imb16bit.min())
        self.assertAlmostEqual(313.811611414, imb16bit.mean(), 2)

        self.assertEqual(255, imb8bit.max())
        self.assertEqual(0, imb8bit.min())
        self.assertAlmostEqual(2.4263832569122314, imb8bit.mean(), 2)
        self.assertAlmostEqual(190.274273157, imb8bit2.mean(), 2)

        self.assertEqual(3, rgb_img.shape[-1])

    def test_threshold(self):
        file = os.path.split(os.path.realpath(__file__))[0] + "/test_images/2013.tif"
        img = fabio.open(file).data
        imb8bit = get8bitImage(img, 0, 20000)
        thresh = thresholdImg(imb8bit, 0.13)
        black_percent = 1. * sum(list(row).count(0) for row in thresh) / (thresh.shape[1] * thresh.shape[0])
        white_percent = 1. * sum(list(row).count(255) for row in thresh) / (thresh.shape[1] * thresh.shape[0])
        self.assertTrue(0.13 - black_percent < 0.01)
        self.assertTrue(0.13 + white_percent - 1. < 0.01)