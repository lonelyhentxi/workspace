import multiprocessing
import shutil
from os import path

from exp2.path_service import PathService
from .pre import Exp2Assistant

from autosklearn.metrics import accuracy
from autosklearn.classification import AutoSklearnClassifier
from autosklearn.constants import *

from sklearn.externals import joblib

tmp_folder = './tmp/autosklearn_parallel_example_tmp'
output_folder = './tmp/autosklearn_parallel_example_out'
path_service = PathService()

for directory in [tmp_folder, output_folder]:
    try:
        shutil.rmtree(directory)
    except OSError as e:
        pass


def get_spawn_classifier(X_train, y_train):
    def spawn_classifier(seed, dataset_name):
        """Spawn a subprocess.

        auto-sklearn does not take care of spawning worker processes. This
        function, which is called several times in the main block is a new
        process which runs one instance of auto-sklearn.
        """

        # Use the initial configurations from meta-learning only in one out of
        # the four processes spawned. This prevents auto-sklearn from evaluating
        # the same configurations in four processes.
        if seed == 0:
            initial_configurations_via_metalearning = 25
            smac_scenario_args = {}
        else:
            initial_configurations_via_metalearning = 0
            smac_scenario_args = {'initial_incumbent': 'RANDOM'}

        # Arguments which are different to other runs of auto-sklearn:
        # 1. all classifiers write to the same output directory
        # 2. shared_mode is set to True, this enables sharing of data between
        # models.
        # 3. all instances of the AutoSklearnClassifier must have a different seed!
        automl = AutoSklearnClassifier(
            time_left_for_this_task=120,  # sec., how long should this seed fit process run
            per_run_time_limit=120,  # sec., each model may only take this long before it's killed
            ml_memory_limit=1024,  # MB, memory limit imposed on each call to a ML algorithm
            shared_mode=True,  # tmp folder will be shared between seeds
            tmp_folder=tmp_folder,
            output_folder=output_folder,
            delete_tmp_folder_after_terminate=False,
            ensemble_size=0,  # ensembles will be built when all optimization runs are finished
            initial_configurations_via_metalearning=initial_configurations_via_metalearning,
            seed=seed,
            smac_scenario_args=smac_scenario_args,
        )
        automl.fit(X_train, y_train, dataset_name=dataset_name)

    return spawn_classifier


def multithread_tiny():
    stage2assistant = Exp2Assistant(stage=2)
    X_train = stage2assistant.train_data
    y_train = stage2assistant.train_label

    processes = []
    spawn_classifier = get_spawn_classifier(X_train, y_train)
    # spawn_classifier = get_spawn_classifier(X_train, y_train)
    for i in range(4):  # set this at roughly half of your cores
        p = multiprocessing.Process(target=spawn_classifier, args=(i, 'label'))
        p.start()
        processes.append(p)
    for p in processes:
        p.join()

    print('Starting to build an ensemble!')
    automl = AutoSklearnClassifier(
        time_left_for_this_task=120,
        per_run_time_limit=120,
        ml_memory_limit=1024,
        shared_mode=True,
        ensemble_size=50,
        ensemble_nbest=300,
        tmp_folder=tmp_folder,
        output_folder=output_folder,
        initial_configurations_via_metalearning=0,
        seed=1,
    )

    # Both the ensemble_size and ensemble_nbest parameters can be changed now if
    # necessary
    automl.fit_ensemble(
        y_train,
        task=MULTICLASS_CLASSIFICATION,
        metric=accuracy,
        precision='32',
        dataset_name='label',
        ensemble_size=20,
        ensemble_nbest=60,
    )

    joblib.dump(automl, path.join(path_service.get_resource(path.join("exp2", "model")), "stage2_model.joblib"))


def simple():
    stage2assistant = Exp2Assistant(stage=2)
    train_data = stage2assistant.train_data
    X_train = train_data.iloc[:, :-1]
    y_train = train_data.iloc[:, -1]

    automl = AutoSklearnClassifier()  # change the time, in this experiment, 1h 12h 24h 48h
    automl.fit(X_train, y_train)
    joblib.dump(automl, path.join(path_service.get_resource("model"), "stage2_model.joblib"))


if __name__ == '__main__':
    simple()
