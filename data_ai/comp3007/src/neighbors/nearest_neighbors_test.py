from numpy import ndarray, asarray
import unittest

from .nearest_neighbors import NearestNeighborsClassifier, NearestNeighborsRegressor


class TestNearestNeighborsClassifier(unittest.TestCase):
    @staticmethod
    def create_data_set() -> (ndarray, ndarray):
        group = asarray([[1.0, 1.1], [1.0, 1.0], [0, 0], [0, 0.1]])
        labels = asarray(['A', 'A', 'B', 'B'])
        return group, labels

    def test_predict(self):
        cls = NearestNeighborsClassifier(k=3)
        dataset, labels = self.create_data_set()
        cls.fit(dataset, labels)
        predictions = cls.predict(asarray([[0, 0]]))
        self.assertEqual(predictions[0], 'B')


class TestNearestNeighborsRegressor(unittest.TestCase):
    @staticmethod
    def create_data_set() -> (ndarray, ndarray):
        group = asarray([[1.0, 1.1, 1.0], [1.0, 1.0, 1.0], [0, 0, 0], [0, -0.1, 0]])
        y = asarray([1.1, 1.0, 0, 0.1])
        return group, y

    def test_predict(self):
        cls = NearestNeighborsRegressor(k=2)
        dataset, labels = self.create_data_set()
        cls.fit(dataset, labels)
        predictions = cls.predict(asarray([[0.5, 0.5, 0.5]]))
        self.assertAlmostEqual(predictions[0], 0.5)
