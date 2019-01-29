from typing import List


def indices_recursively(n: int, d: int) -> List[List[int]]:
    if d == 0:
        return [[0] * n]
    elif n == 1:
        return [[d]]
    elif n == 2:
        return [[i, d - i] for i in range(0, d + 1)]
    else:
        solutions = []
        if n > 2:
            for i in range(0, d + 1):
                before = indices_recursively(n - 1, d - i)
                for elem in before:
                    elem.append(i)
                    if sum(elem) == d:
                        solutions.append(elem)
        return solutions


def indices(n: int, d: int) -> List[List[int]]:
    return indices_recursively(n, d)