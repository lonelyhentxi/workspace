import math

from numpy import ndarray


def euclidean(x: ndarray, y: ndarray) -> float:
    return math.sqrt(sum(list(map(lambda item: pow(item[1] - y[item[0]], 2), enumerate(x)))))
