import numpy as np
from numpy import matrix, ndarray
from .functions import gauss_func


class GaussianNB:

    def __init__(self):
        self.classes = None
        self.classes_count = None
        self.mean: matrix = None
        self.var: matrix = None

    def fit(self, X_train: matrix, y_train: ndarray):
        self.classes, self.classes_count = np.unique(y_train, return_counts=True)
        self.mean = np.zeros((self.classes_count.shape[0], X_train.shape[1]), dtype=np.float64)
        self.var = np.zeros((self.classes_count.shape[0], X_train.shape[1]), dtype=np.float64)
        for index, label in enumerate(self.classes):
            x_i = X_train[y_train == label]
            self.mean[index, :] = np.mean(x_i, axis=0)
            self.var[index, :] = np.var(x_i, axis=0)
        return self

    def predict(self, X_test: matrix) -> ndarray:
        likehood = []
        for i in range(self.classes.shape[0]):
            likehood.append(self.classes_count[i] * gauss_func(X_test, self.mean[i, :], self.var[i, :]))
        likehood = np.asarray(likehood).T
        return np.argmax(likehood, axis=1)