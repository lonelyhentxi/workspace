from os import path

from exp2.path_service import PathService
from .pre import Exp2Assistant

import sklearn.metrics

from autosklearn.classification import AutoSklearnClassifier
from autosklearn.constants import *

from sklearn.externals import joblib

path_service = PathService()
stage1assistant = Exp2Assistant(stage=1)
X_train = stage1assistant.train_data
y_train = stage1assistant.train_label
X_test = stage1assistant.test_data

automl: AutoSklearnClassifier = joblib.load(
    path.join(path_service.get_resource(path.join("exp2", "model")), "stage1_model.joblib")
)
pre_train = automl.predict(X_train)
pre_test = automl.predict(X_test)

print("train mean squared error", sklearn.metrics.mean_squared_error(y_train, pre_train) / 2)

stage1assistant.save_label(pre_test)
print(automl.show_models())
