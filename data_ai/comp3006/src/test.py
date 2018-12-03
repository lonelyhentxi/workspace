import pandas as pd
import numpy as np
from os import path
from path_service import LOG_DIR, DATA_DIR
from sklearn.metrics import log_loss
import re

prob_columns = list(map(lambda x: f"prob{x}", range(8)))
prob_columns_without_end = list(map(lambda x: f"prob{x}", range(7)))

def row_check(df: pd.DataFrame):
    df.loc[:,prob_columns]=df.loc[:,prob_columns].apply(lambda x: x/np.sum(x),axis=1,result_type='expand')
    df = df.round(5)
    sum7 = np.sum(df.loc[:,prob_columns_without_end],axis=1)
    df.loc[:,'prob7'] = 1.0 - sum7
    return df


def get_prob_res(file_name: str):
    df: pd.DataFrame = pd.DataFrame([])
    with open(path.join(LOG_DIR, file_name), 'r') as prob_file:
        prob_lines = prob_file.readlines()
    probs = {}
    for i in range(8):
        probs[i] = []
    for line in prob_lines:
        words = re.split(r"\s", line)
        for i in range(8):
            pos = i * 2
            prob_index = int(words[pos][-1])
            probs[prob_index].append(float(words[pos + 1]))
    df.loc[:, "file_id"] = pd.Series(list(range(1, len(probs[0]) + 1)), dtype=np.int)
    for i in range(8):
        df.loc[:, f"prob{i}"] = pd.Series(probs[i], dtype=np.float)
    return row_check(df)


def get_single_res(file_name: str, true_mode: bool = True):
    df: pd.DataFrame = pd.DataFrame([])
    with open(path.join(LOG_DIR if not true_mode else DATA_DIR, file_name), 'r') as prob_file:
        prob_lines = prob_file.readlines()
    probs = {}
    for i in range(8):
        probs[i] = []
    j = 0
    for line in prob_lines:
        label = int(str.strip(re.split(r"\s", line)[0])[-1])
        for i in range(8):
            if i == label:
                probs[i].append(1.0)
            else:
                probs[i].append(0.0)
    df.loc[:, "file_id"] = pd.Series(list(range(1, len(probs[0]) + 1)), dtype=np.int)
    for i in range(8):
        df.loc[:, f"prob{i}"] = pd.Series(probs[i], dtype=np.float)
    return df


def get_probs(df: pd.DataFrame) -> pd.DataFrame:
    return df.loc[:, list(map(lambda x: f"prob{x}", range(8)))]


def check_valid_log_loss():
    valid_prob_df = get_prob_res('valid_prob.log')
    labels = get_single_res('security.valid', True)
    print("prob mode: ", log_loss(get_probs(labels), get_probs(valid_prob_df)))


def check_train_log_loss():
    valid_prob_df = get_prob_res('train_prob.log')
    labels = get_single_res('new_train', True)
    print("prob mode: ", log_loss(get_probs(labels), get_probs(valid_prob_df)))


def save_train_res(df: pd.DataFrame):
    df.to_csv(path.join(DATA_DIR, "test_submit.csv"), sep=",", index=False, float_format='%.5f')


if __name__ == "__main__":
    check_valid_log_loss()
    check_train_log_loss()
    test_prob_df = get_prob_res("test_prob.log")
    save_train_res(test_prob_df)
    df = pd.read_csv(path.join(DATA_DIR, "test_submit.csv"), sep=",")
    for index, row in df.iterrows():
        if np.abs(np.sum(row[list(map(lambda x: f"prob{x}", range(8)))]) - 1.0) > 1e-6:
            raise Exception(f"sum prob not equal 1.0 in {index}")
