import numpy as np


class BaggingClassifier:
    def __init__(self, basics):
        self.basics = basics

    def choose_set(self, X_):
        indexs = np.random.choice(X_.shape[0], X_.shape[0])
        return X_[indexs]

    @staticmethod
    def vote(labels):
        labels, labels_count = np.unique(labels, return_counts=True)
        return labels[np.argmax(labels_count)]

    def fit(self, X, y):
        X_ = np.c_[X, y]
        for i in self.basics:
            new_X_ = self.choose_set(X_)
            i.fit(new_X_[:, :-1], new_X_[:, -1])
        return self

    def predict_one(self, x):
        labels = np.array([i.predict_one(x) for i in self.basics])
        return self.vote(labels)

    def predict(self, X):
        tmp = np.array([clf.predict(X) for clf in self.basics]).T
        return np.array([self.vote(i) for i in tmp])
