__author__ = 'Jiranun.J'

import unittest
from musclex.modules.EquatorImage import EquatorImage
import os

dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"

class MyTestCase(unittest.TestCase):
# class MyTestCase():

    def test_bio_image1(self):

        bio_image = EquatorImage(dir_path, "2013.tif")
        settings = {
            'nPeaks' : 5,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': True,
            'blank_mask':False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertTrue('avg_ratio' in actual_result)

    def test_bio_image2(self):

        bio_image = EquatorImage(dir_path, "2014.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'blank_mask':False,
            'isSkeletal': False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertTrue('avg_ratio' in actual_result)

    def test_bio_image3(self):

        bio_image = EquatorImage(dir_path, "15peaks.tif")
        settings = {
            'nPeaks' : 15,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'blank_mask':False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertTrue('avg_ratio' in actual_result)

    def test_bio_image4(self):

        bio_image = EquatorImage(dir_path, "2016_1.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'blank_mask':False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertTrue('avg_ratio' in actual_result)

    def test_bio_image5(self):

        bio_image = EquatorImage(dir_path, "2016_2.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'blank_mask':False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertTrue('avg_ratio' in actual_result)

    def test_bio_image6(self):

        bio_image = EquatorImage(dir_path, "2016_3.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'blank_mask':False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertTrue('avg_ratio' in actual_result)

    def test_bio_image7(self):

        bio_image = EquatorImage(dir_path, "2016_4.tif")
        settings = {
            'center':(1022, 1002),
            'rotationAngle':61, # TODO
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'blank_mask':False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertTrue('avg_ratio' in actual_result)

    def test_bio_image8(self):

        bio_image = EquatorImage(dir_path, "venus8int.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'blank_mask':False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertTrue('avg_ratio' in actual_result)

    def test_bio_image9(self):

        bio_image = EquatorImage(dir_path, "venus32int.tif")
        settings = {
            'nPeaks' : 2,
            'model' : 'Voigt',
            'left_sigmac' : 1.0,
            'right_sigmac' : 1.0,
            'isSkeletal': False,
            'blank_mask':False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertTrue('avg_ratio' in actual_result)

    def test_bio_image10(self):

        bio_image = EquatorImage(dir_path, "P34_2_2_pla.tif")
        settings = {
            'nPeaks': 2,
            'model': 'Voigt',
            'left_sigmac': 1.0,
            'right_sigmac': 1.0,
            'isSkeletal': False,
            'blank_mask':False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertTrue('avg_ratio' in actual_result)