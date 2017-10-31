__author__ = 'Jiranun.J'

import unittest
from musclex.modules.BioImage import BioImage
import os

dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"

class MyTestCase(unittest.TestCase):
# class MyTestCase():
    def test_bio_image1(self):

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
        self.assertAlmostEqual(0.6716248307522783, actual_result['avg_ratio'], 2)

    def test_bio_image3(self):

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
        self.assertAlmostEqual(0.56320354706431053, actual_result['avg_ratio'], 2)

    def test_bio_image5(self):

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
        self.assertAlmostEqual(0.61797479108855469, actual_result['avg_ratio'], 2)

    def test_bio_image6(self):

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
        self.assertAlmostEqual(0.24289362330051062, actual_result['avg_ratio'], 2)

    def test_bio_image7(self):

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
        self.assertAlmostEqual(0.26736174197446416, actual_result['avg_ratio'], 2)

    def test_bio_image8(self):

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
        self.assertAlmostEqual(2.0784631577443942, actual_result['avg_ratio'], 2)

    def test_bio_image9(self):

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
        self.assertAlmostEqual(2.6254843091209827, actual_result['avg_ratio'], 2)

    def test_bio_image10(self):

        bio_image = BioImage(dir_path, "P34_2_2_pla.tif")
        settings = {
            'nPeaks': 2,
            'model': 'Voigt',
            'left_sigmac': 1.0,
            'right_sigmac': 1.0,
            'isSkeletal': False,
            'no_cache': True
        }
        bio_image.process(settings)
        actual_result = bio_image.info["fit_results"]
        # print actual_result['avg_ratio']
        self.assertAlmostEqual(0.51448509463, actual_result['avg_ratio'], 2)