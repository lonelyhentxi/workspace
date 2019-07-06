import numpy as np
from numpy import ndarray


def common_gauss(c: ndarray, x: ndarray, mu: ndarray):
    c2 = c@c
    numerator = - np.exp(np.sum((x - mu) ** 2, axis=1) / (2 * c2))
    return numerator / np.sqrt(2 * np.pi * c2)
