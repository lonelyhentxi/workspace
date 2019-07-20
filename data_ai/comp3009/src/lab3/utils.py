import pandas as pd
import numpy as np
from typing import Dict, Any, Union, Iterable
from numpy import ndarray
import math
import copy


def one_hot_encoder(labels: pd.Series) -> Dict[Any, ndarray]:
    ret = {}
    uniques = labels.unique()
    unique_num = len(uniques)
    for index, label in enumerate(uniques):
        ret[label] = np.zeros(unique_num).astype(np.int, copy=False)
        ret[label][index] = 1
    return ret


def value_encoder(labels: pd.Series) -> Dict[Any, int]:
    ret = {}
    uniques = labels.unique()
    for index, label in enumerate(uniques):
        ret[label] = index
    return ret


def data_split(dataset: pd.DataFrame, split_rate: float = 0.8, method: str = "value") -> \
        (ndarray, ndarray, ndarray, ndarray, Union[Dict[Any, ndarray], Dict[Any, int]]):
    row_num, col_num = dataset.shape
    split_point = int(row_num * split_rate)
    x_train = dataset.iloc[:split_point, :-1].as_matrix(columns=None).astype(np.float64, copy=False)
    x_test = dataset.iloc[split_point:, :-1].as_matrix(columns=None).astype(np.float64, copy=False)
    factor_mapper: Union[Dict[Any, ndarray], Dict[Any, int]] = None
    y_train, y_test = None, None
    if method == "value":
        factor_mapper = value_encoder(dataset.iloc[:, -1])
        y_train = np.asarray(list(map(lambda x: factor_mapper[x], dataset.iloc[:split_point, -1])), dtype=np.int)
        y_test = np.asarray(list(map(lambda x: factor_mapper[x], dataset.iloc[split_point:, -1])), dtype=np.int)
    elif method == "one-hot":
        factor_mapper = one_hot_encoder(dataset.iloc[:, -1])
        y_train = pd.DataFrame(list(map(lambda x: factor_mapper[x], dataset.iloc[:split_point, -1])),
                               dtype=np.int).as_matrix(
            columns=None)
        y_test = pd.DataFrame(list(map(lambda x: factor_mapper[x], dataset.iloc[split_point:, -1])), dtype=np.int) \
            .as_matrix(
            columns=None)
    else:
        raise TypeError("invalid method")
    return x_train, y_train, x_test, y_test, factor_mapper


def data_indent(dataset: pd.DataFrame, method: str = "value") -> \
        (ndarray, ndarray, Union[Dict[Any, ndarray], Dict[Any, int]]):
    x_train = dataset.iloc[:, :-1].as_matrix(columns=None).astype(np.float64, copy=False)
    factor_mapper: Union[Dict[Any, ndarray], Dict[Any, int]] = None
    y_train = None
    if method == "value":
        factor_mapper = value_encoder(dataset.iloc[:, -1])
        y_train = np.asarray(list(map(lambda x: factor_mapper[x], dataset.iloc[:, -1])), dtype=np.int)
    elif method == "one-hot":
        factor_mapper = one_hot_encoder(dataset.iloc[:, -1])
        y_train = pd.DataFrame(list(map(lambda x: factor_mapper[x], dataset.iloc[:, -1])),
                               dtype=np.int).as_matrix(
            columns=None)
    else:
        raise TypeError("invalid method")
    return x_train, y_train, factor_mapper


def classification_score(classification, X_train: ndarray, y_train: ndarray, X_test: ndarray, y_test: ndarray,
                         out_format: str = "value"):
    print(f"{classification.__class__}开始训练...")
    trained_classification = classification.fit(X_train, y_train)
    print(f"{classification.__class__}完成训练")
    print(f"{classification.__class__}开始测试...")
    pred_test: ndarray = trained_classification.predict(X_test)
    print(f"{classification.__class__}完成测试")
    print(f"{classification.__class__}开始评分...")
    count = 0
    if out_format == "one-hot":
        pred_rounded = np.asarray(list(map(round, pred_test.flatten()))).reshape(y_test.shape)
        for index, item in enumerate(pred_rounded):
            add_value = 1
            for j, jt in enumerate(item):
                if jt != y_test[index, j]:
                    add_value = 0
                    break
            count += add_value
    else:
        for index, item in enumerate(pred_test):
            if item == y_test[index]:
                count += 1
    print(f"{classification.__class__}完成评分")
    return count / len(pred_test)


def classification_cross_val_score(classification, X: ndarray, y: ndarray, cv: int = 10,
                                   out_format: str = "value") -> list:
    result_score = []
    num = len(y)
    groups = []
    group_num = int(num / cv - 1)
    for i in range(cv - 1):
        groups.append(list(range(i * group_num, (i + 1) * group_num)))
    groups.append(list(range(cv - 1 * group_num, num)))
    for i in range(cv):
        x_tests = X[groups[i]]
        y_tests = y[groups[i]]
        others = []
        for index, group in enumerate(groups):
            if index != i:
                others = others + group
        x_trains = X[others]
        y_trains = y[others]
        print(f"{classification.__class__}开始第{i+1}折检验流程...")
        result_score.append(
            classification_score(copy.deepcopy(classification), x_trains, y_trains, x_tests, y_tests, out_format))
        print(f"{classification.__class__}完成第{i+1}折检验流程")
    return result_score


def count(elements: Iterable[Any]) -> Dict[Any, int]:
    ret: Dict[Any, int] = {}
    for it in elements:
        if ret.get(it) is None:
            ret[it] = 0
        ret[it] += 1
    return ret
