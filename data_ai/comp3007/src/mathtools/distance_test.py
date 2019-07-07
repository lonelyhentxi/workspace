import unittest
from .distance import *
from numpy import asarray


class TestDistance(unittest.TestCase):
    def test_distance(self):
        res1 = euclidean(asarray([3, 4]), asarray([0, 0]))
        self.assertAlmostEqual(res1, 5)
