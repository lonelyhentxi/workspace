import numpy as np
from numpy import ndarray


def min_max_normalize(X: ndarray, new_max: float = 1.0,
                      new_min: float = 0.0) -> ndarray:
    new_X: ndarray = X.copy().astype(np.float)
    for i in range(new_X.shape[1]):
        col = new_X[:, i]
        old_max = np.max(col)
        old_min = np.min(col)
        if old_max - old_min == 0:
            new_X[:, i] = (new_max - new_min) / 2.0
        else:
            times = (new_max - new_min) / (old_max - old_min)
            new_X[:, i] = new_min + (col - old_min) * times
    return new_X
