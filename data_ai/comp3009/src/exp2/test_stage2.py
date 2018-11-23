from os import path

from exp2.path_service import PathService
from .pre import Exp2Assistant

import sklearn.metrics

from autosklearn.classification import AutoSklearnClassifier
from autosklearn.constants import *

path_service = PathService()
from sklearn.externals import joblib

stage2assistant = Exp2Assistant(stage=2)
X_train = stage2assistant.train_data
y_train = stage2assistant.train_label
X_test = stage2assistant.test_data

automl: AutoSklearnClassifier = joblib.load(
    path.join(path_service.get_resource(path.join("exp2", "model")), "stage2_model.joblib"))
pre_train = automl.predict(X_train)
pre_test = automl.predict(X_test)
print("f1 score train", sklearn.metrics.f1_score(y_train, pre_train))
stage2assistant.save_label(pre_test)
print(automl.show_models())
