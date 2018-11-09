import numpy as np


def gauss_func(x, mu, singma):
    sqsingma = singma @ singma
    numerator = -np.exp(np.sum((x - mu) ** 2, axis=1) / (2 * sqsingma))
    return numerator / np.sqrt(2 * np.pi * sqsingma)
