__author__ = 'Jiranun.J'

import unittest
from musclex.modules.BioImage import BioImage
import os


class MyTestCase(unittest.TestCase):
# class MyTestCase():
    def test_bio_image1(self):
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        bio_image = BioImage(dir_path, "2013.tif")
        settings = {
            'nPeaks' : 5,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': True,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertAlmostEqual(0.260564419777, actual_result['avg_ratio'], 2)

    def test_bio_image2(self):
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        bio_image = BioImage(dir_path, "2014.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertAlmostEqual(0.654690274379, actual_result['avg_ratio'], 2)

    def test_bio_image3(self):
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        bio_image = BioImage(dir_path, "15peaks.tif")
        settings = {
            'nPeaks' : 15,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertAlmostEqual(0.123585500238, actual_result['avg_ratio'], 2)

    def test_bio_image4(self):
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        bio_image = BioImage(dir_path, "2016_1.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertAlmostEqual(0.576788385354, actual_result['avg_ratio'], 2)

    def test_bio_image5(self):
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        bio_image = BioImage(dir_path, "2016_2.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertAlmostEqual(0.605730390685, actual_result['avg_ratio'], 2)

    def test_bio_image6(self):
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        bio_image = BioImage(dir_path, "2016_3.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertAlmostEqual(0.25435649452, actual_result['avg_ratio'], 2)

    def test_bio_image7(self):
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        bio_image = BioImage(dir_path, "2016_4.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertAlmostEqual(0.250585006997, actual_result['avg_ratio'], 2)

    def test_bio_image8(self):
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        bio_image = BioImage(dir_path, "venus8int.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertAlmostEqual(2.03203701811, actual_result['avg_ratio'], 2)

    def test_bio_image9(self):
        dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
        bio_image = BioImage(dir_path, "venus32int.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertAlmostEqual(2.69400151596, actual_result['avg_ratio'], 2)