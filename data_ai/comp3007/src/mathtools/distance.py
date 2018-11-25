import math

from numpy import ndarray
from typing import Union


def euclidean(x: ndarray, y: ndarray) -> Union[int, float]:
    return math.sqrt(sum(list(map(lambda item: pow(item[1] - y[item[0]], 2), enumerate(x)))))
