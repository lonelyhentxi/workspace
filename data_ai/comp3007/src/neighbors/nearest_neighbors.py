from collections import Counter
import numpy as np
from numpy import ndarray, asarray
from typing import Callable, Union, Any
from mathtools.distance import euclidean


class NearestNeighbors:
    def __init__(self,
                 predictor: Callable[[ndarray], Union[int, Any, float]],
                 k: int = 1,
                 diff_func: Callable[[ndarray, ndarray], Union[float, int]] = euclidean
                 ):
        self.k = k
        self.diff_func = diff_func
        self.X = None
        self.y = asarray([])
        self.predictor = predictor
        self.fit_flag = False

    def fit(self, X: ndarray, y: ndarray):
        if self.predictor is None:
            raise Exception("Must define predictor, or use classifier or regressor")
        if self.X is None:
            self.X = X
        else:
            self.X = np.append(self.X, X)
        self.y = np.append(self.y, y, axis=0)
        self.fit_flag = True

    def predict(self, X: ndarray) -> ndarray:
        if not self.fit_flag:
            raise Exception("No fit!")
        predictions = asarray([])
        for row in np.asarray(X):
            diff_res = asarray(
                list(map(lambda x: self.diff_func(x, row), np.asarray(self.X))))
            diff_res_sort_index = np.argsort(diff_res).take(range(min(self.k, self.y.shape[0])))
            prediction = self.predictor(self.y[diff_res_sort_index])
            predictions = np.append(predictions, prediction)
        return predictions


class NearestNeighborsClassifier(NearestNeighbors):
    @staticmethod
    def _predictor(k_nearest_y: ndarray) -> Any:
        nearest_y = Counter(k_nearest_y.tolist()).most_common()
        return nearest_y

    def __init__(self, **kwargs):
        super().__init__(predictor=self._predictor, **kwargs)


class NearestNeighborsRegressor(NearestNeighbors):
    @staticmethod
    def _predictor(k_nearest_y: ndarray) -> Union[int, float]:
        prediction: Union[int, float] = np.mean(k_nearest_y)
        return prediction

    def __init__(self, **kwargs):
        super().__init__(predictor=self._predictor, **kwargs)
