import numpy as np


class Tree:
    def __init__(self):
        self.feature = None
        self.label = None
        self.n_samples = None
        self.gain = None
        self.left = None
        self.right = None
        self.threshold = None
        self.depth = 0

    def build(self, features, target, method: str = "gini"):
        self.n_samples = features.shape[0]

        if len(np.unique(target)) == 1:
            self.label = target[0]
            return

        best_gain = 0.0
        best_feature = None
        best_threshold = None

        if method in {'gini', 'entropy', 'error'}:
            self.label = max([(c, len(target[target == c])) for c in np.unique(target)],
                             key=lambda x: x[1])[0]
        else:
            self.label = np.mean(target)

        impurity_node = self._calc_impurity(method, target)

        for col in range(features.shape[1]):
            feature_level = np.unique(features[:, col])
            thresholds = (feature_level[:-1] + feature_level[1:]) / 2.0

            for threshold in thresholds:
                target_l = target[features[:, col] <= threshold]
                impurity_l = self._calc_impurity(method, target_l)
                n_l = float(target_l.shape[0]) / self.n_samples

                target_r = target[features[:, col] > threshold]
                impurity_r = self._calc_impurity(method, target_r)
                n_r = float(target_r.shape[0]) / self.n_samples

                ig = impurity_node - (n_l * impurity_l + n_r * impurity_r)

                if ig > best_gain:
                    best_gain = ig
                    best_feature = col
                    best_threshold = threshold

        self.feature = best_feature
        self.gain = best_gain
        self.threshold = best_threshold
        self._divide_tree(features, target, method)

    def _divide_tree(self, features, target, criterion):
        features_l = features[features[:, self.feature] <= self.threshold]
        target_l = target[features[:, self.feature] <= self.threshold]
        self.left = Tree()
        self.left.depth = self.depth + 1
        self.left.build(features_l, target_l, criterion)

        features_r = features[features[:, self.feature] > self.threshold]
        target_r = target[features[:, self.feature] > self.threshold]
        self.right = Tree()
        self.right.depth = self.depth + 1
        self.right.build(features_r, target_r, criterion)

    def _calc_impurity(self, criterion, target):
        c = np.unique(target)
        s = target.shape[0]

        if criterion == 'gini':
            return self._gini(target, c, s)
        elif criterion == 'entropy':
            return self._entropy(target, c, s)
        elif criterion == 'error':
            return self._error(target, c, s)
        elif criterion == 'mse':
            return self._mse(target)
        else:
            return self._gini(target, c, s)

    def _gini(self, target, n_classes, n_samples):
        gini_index = 1.0
        gini_index -= sum([(float(len(target[target == c])) / float(n_samples)) ** 2.0 for c in n_classes])
        return gini_index

    def _entropy(self, target, n_classes, n_samples):
        entropy = 0.0

        for c in n_classes:
            p = float(len(target[target == c])) / n_samples
            if p > 0.0:
                entropy -= p * np.log2(p)
        return entropy

    def _error(self, target, n_classes, n_samples):
        return 1.0 - max([len(target[target == c]) / n_samples for c in n_classes])

    def _mse(self, target):
        y_hat = np.mean(target)
        return np.mean((target - y_hat) ** 2.0)

    def predict(self, d):
        if self.feature is not None:
            if d[self.feature] <= self.threshold:
                return self.left.predict(d)
            else:
                return self.right.predict(d)
        else:
            return self.label
