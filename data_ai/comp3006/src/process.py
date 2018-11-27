import multiprocessing
import shutil

import sklearn.model_selection
import sklearn.metrics

# from autosklearn.metrics import f1
# from autosklearn.classification import AutoSklearnClassifier
# from autosklearn.constants import *

from sklearn.externals import joblib
from os import path
from path_service import TMP_DIR, DATA_DIR
import pandas as pd
import numpy as np

tmp_dir = path.join(TMP_DIR, "autosklearn_parallel_tmp")
output_dir = path.join(TMP_DIR, "autosklearn_parallel_out")

for dir in [tmp_dir, output_dir]:
    try:
        shutil.rmtree(dir)
    except OSError as e:
        pass

"""
def get_spawn_classifier(X, y):
    def spawn_classifier(seed, dataset_name):
        if seed == 0:
            initial_configuration_via_metalearning = 25
            smac_scenario_args = {}
        else:
            initial_configuration_via_metalearning = 0
            smac_scenario_args = {"initial_incumbent": "RANDOM"}
        automl = AutoSklearnClassifier(
            time_left_for_this_task=86400 * 2,
            per_run_time_limit=3600,
            ml_memory_limit=10240,
            shared_model=True,
            tmp_folder=tmp_dir,
            output_dir=output_dir,
            ensemble_size=0,
            initial_configuration_via_metalearning=initial_configuration_via_metalearning,
            seed=seed,
            smac_scenario_args=smac_scenario_args
        )
        automl.fit(X, y, dataset_name=dataset_name)

    return spawn_classifier
"""

label_list = ["prob0", "prob1", "prob2", "prob3", "prob4", "prob5", "prob6", "prob7"]


def main():
    raw_df = pd.read_csv(path.join(DATA_DIR, "security_train.csv"))
    raw_df.loc[["prob0", "prob1", "prob2", "prob3", "prob4", "prob5", "prob6", "prob7"]] = 0.

    X_train, X_test, y_train, y_test = sklearn.model_selection.train_test_split(X, y, random_state=1, test_size=0.1)


def transform():
    raw_df = pd.read_csv(path.join(DATA_DIR, "security_train1.csv"), chunksize=10, iterator=True)
    for chunk_index, chunk in enumerate(raw_df):
        for index, label_column in enumerate(label_list):
            chunk[label_column] = pd.Series(chunk["label"] == index, dtype=np.float64)
        chunk.to_csv(path.join(DATA_DIR, "security_train1_transformed.csv"), index=False,
                     mode="a+" if chunk_index != 0 else "w",
                     header=(chunk_index == 0))


if __name__ == "__main__":
    transform()
