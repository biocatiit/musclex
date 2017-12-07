__author__ = 'Jiranun.J'

import unittest
from musclex.modules.ProjectionProcessor import ProjectionProcessor
import os
from os.path import join
import pickle
dir_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images"
settings = pickle.load(open(join(dir_path, 'boxes_peaks.info'), "rb"))
settings['no_cache'] = True

class MyTestCase(unittest.TestCase):
# class MyTestCase():

    def test_pt_1(self):
        # Test multiple boxes, peaks, box type, and bg sub type
        projProc = ProjectionProcessor(dir_path, "p60_1_cont.tif.result.tif")
        settings = pickle.load(open(join(dir_path, 'boxes_peaks.info'), "rb"))
        settings['no_cache'] = True
        projProc.process(settings)

        info = projProc.info

        self.assertTrue('fit_results' in info)
        self.assertTrue('baselines' in info)
        self.assertTrue('centroids' in info)
        self.assertTrue('widths' in info)

        boxes = info['boxes']

        for bn in boxes.keys():
            self.assertTrue(bn in info['fit_results'])
            self.assertTrue( bn in info['baselines'])
            self.assertTrue(bn in info['centroids'])
            self.assertTrue(bn in info['widths'])

    def test_pt_2(self):
        # Test multiple boxes, peaks, box type, and bg sub type
        projProc = ProjectionProcessor(dir_path, "p60_1_rest.tif.result.tif")
        settings = pickle.load(open(join(dir_path, 'boxes_peaks.info'), "rb"))
        settings['no_cache'] = True
        projProc.process(settings)

        info = projProc.info

        self.assertTrue('fit_results' in info)
        self.assertTrue('baselines' in info)
        self.assertTrue('centroids' in info)
        self.assertTrue('widths' in info)

        boxes = info['boxes']

        for bn in boxes.keys():
            self.assertTrue(bn in info['fit_results'])
            self.assertTrue(bn in info['baselines'])
            self.assertTrue(bn in info['centroids'])
            self.assertTrue(bn in info['widths'])

