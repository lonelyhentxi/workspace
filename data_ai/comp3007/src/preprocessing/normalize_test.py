import unittest
from .normalize import min_max_normalize
import numpy as np


class TestNormalize(unittest.TestCase):
    def test_min_max_normalize(self):
        X = np.asarray([[1, 2, 3], [1, 2, 3]])
        new_X = min_max_normalize(X)
        # check divide zero
        self.assertEqual(np.array_equal(
            np.asarray([[0.5, 0.5, 0.5], [0.5, 0.5, 0.5]]), new_X), True)
        # check return new value not change old
        self.assertEqual(np.array_equal(
            np.asarray([[1, 2, 3], [1, 2, 3]]), X), True)
        X1 = np.asarray([[500, 2], [-500, -2], [0, 0.]])
        new_X1 = min_max_normalize(X1, new_max=1, new_min=-1)
        # check common condition
        self.assertEqual(np.array_equal(
            new_X1, np.asarray([[1, 1], [-1, -1], [0, 0]], dtype=np.float)), True)
