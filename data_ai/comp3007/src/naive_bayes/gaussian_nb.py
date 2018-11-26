from numpy import asarray, ndarray
import numpy as np
from collections import Counter
from mathtools.signal import common_gauss


class GaussianNaiveBayesClassifier:
    def __init__(self):
        self.fit_flag = False
        self.classes: ndarray = None
        self.class_count: ndarray = None
        self.mean: ndarray = None
        self.var: ndarray = None

    def fit(self, X: ndarray, y: ndarray):
        tuple_classes, tuple_class_count = zip(
            *Counter(y.tolist()).items())
        self.classes = asarray(tuple_classes)
        self.class_count = asarray(tuple_class_count)
        self.mean = np.zeros(
            (self.class_count.shape[0], X.shape[1]), dtype=np.float)
        self.var = np.zeros(
            (self.class_count.shape[0], X.shape[1]), dtype=np.float)
        for index, clazz in enumerate(self.classes):
            x_with_class = X[y == clazz]
            self.mean[index, :] = np.mean(x_with_class, axis=0)
            self.var[index, :] = np.var(x_with_class, axis=0)

    def predict(self, X: ndarray) -> ndarray:
        likehood = asarray([], dtype=np.float)
        for index, item in enumerate(self.classes):
            np.append(likehood, self.class_count[index] * common_gauss(X, self.mean[index, :], self.var[index, :]))
        likehood = np.asarray(likehood).T
        return np.argmax(likehood, axis=1)
