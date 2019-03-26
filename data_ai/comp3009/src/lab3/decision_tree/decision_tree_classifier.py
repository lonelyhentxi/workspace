import numpy as np
from numpy import ndarray
from .tree import Tree


class DecisionTreeClassifier:
    def __init__(self, method: str = 'gini', max_depth: int = 3, min_criterion: float = 0.05):
        self.root: Tree = None
        self.method = method
        self.max_depth = max_depth
        self.min_criterion = min_criterion

    def fit(self, X_train: ndarray, y_train: ndarray):
        self.root = Tree()
        self.root.build(X_train, y_train, self.method)
        return self

    def predict(self, X_test: ndarray):
        return np.array([self.root.predict(f) for f in X_test])
