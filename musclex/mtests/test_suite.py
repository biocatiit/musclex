import unittest, time, sys, logging
from eq.equator_image_test import *

logger = logging.getLogger()


class MuscleXTest(unittest.TestCase):

    def testEquatorImage(self):
        """
        Runs a test of EquatorImage using the given settings.
        """
        self.assertTrue(
            equator_image_test(
                pickledir="eq/tmp_verify", inputpath="test_images",
                compdir="eq/test_pickles", testrecord=False,
                testversion="1.14.4", keeppickles=False
                ),
            "Equator Image Test failed."
            )

if __name__=="__main__":
    unittest.main(verbosity=2)
