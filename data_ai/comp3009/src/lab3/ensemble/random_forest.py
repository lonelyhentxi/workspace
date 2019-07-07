import numpy as np
from ..decision_tree.decision_tree_classifier import DecisionTreeClassifier
from .bagging import BaggingClassifier


class RandomForestClassifier(object):
    def __init__(self, tree_num=5, max_depth=6):
        trees = [DecisionTreeClassifier(max_depth=max_depth) for _ in range(tree_num)]
        self.clf = BaggingClassifier(trees)

    def fit(self, X, y):
        self.clf.fit(X, y)
        return self

    def predict(self, X):
        return self.clf.predict(X)
