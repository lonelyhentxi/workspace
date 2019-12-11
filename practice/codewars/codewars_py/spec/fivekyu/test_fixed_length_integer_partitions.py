import unittest
from codewars.fivekyu.fixed_length_integer_partitions import indices


class TestIndices(unittest.TestCase):
    def test_indices(self):
        self.assertSequenceEqual(sorted(indices(3, 2)), sorted([[0, 0, 2],
                                                                [0, 1, 1], [0, 2, 0], [1, 0, 1], [1, 1, 0], [2, 0, 0]]))
