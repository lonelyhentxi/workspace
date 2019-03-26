from os import path

import autosklearn.regression

from exp2.path_service import PathService
from .pre import Exp2Assistant
from sklearn.externals import joblib


def main():
    path_service = PathService()
    stage1assistant = Exp2Assistant(stage=1)
    X = stage1assistant.train_data
    y = stage1assistant.train_label

    automl = autosklearn.regression.AutoSklearnRegressor(
        time_left_for_this_task=3600,
        per_run_time_limit=60,
        ensemble_size=1,
        initial_configurations_via_metalearning=0
    )
    automl.fit(X, y)
    joblib.dump(automl, path.join(path_service.get_resource(path.join("exp2", "model")), "stage1_process.joblib"))


if __name__ == "__main__":
    main()
