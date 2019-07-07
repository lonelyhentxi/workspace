import pandas as pd
from lab3.decision_tree.decision_tree_classifier import DecisionTreeClassifier
from lab3.native_bayes.guassian_nb import GaussianNB
from lab3.nn.mlp_classifier import MLPClassifier
from lab3.utils import data_indent, classification_cross_val_score
from sklearn.utils import shuffle
from lab3.ensemble.bagging import BaggingClassifier
from lab3.ensemble.random_forest import RandomForestClassifier

if __name__ == '__main__':
    # Read data
    print("数据预处理中")
    df: pd.DataFrame = pd.read_csv('./iris.csv', sep=",")
    features = df.columns
    df = shuffle(df)
    X_train, Y_train, mapper = data_indent(df, method="value")
    clf_tree = DecisionTreeClassifier()
    clf_nb = GaussianNB()
    clf_rf = RandomForestClassifier()
    clf_bagging = BaggingClassifier(
        [clf_tree, clf_nb, clf_rf])
    X_train1, Y_train1, mapper1 = data_indent(df, method="one-hot")
    clf_mlp = MLPClassifier(layers=(4, 20, 3), activation='sigmoid', epochs=1500, learning_rate=0.1)
    decision_tree_res = classification_cross_val_score(clf_tree, X_train, Y_train, cv=5)
    gaussian_bayes_res = classification_cross_val_score(clf_nb, X_train, Y_train, cv=5)
    random_forest_res = classification_cross_val_score(clf_rf, X_train, Y_train, cv=5)
    bagging_res = classification_cross_val_score(clf_bagging, X_train, Y_train, cv=5)
    multilayer_perceptron_res = classification_cross_val_score(clf_mlp, X_train1, Y_train1, cv=5, out_format="one-hot")
    print(f"decision tree res :{decision_tree_res}")
    print(f"gaussian_bayes_res {gaussian_bayes_res}")
    print(f"random_forest_res {random_forest_res}")
    print(f"bagging_res {bagging_res}")
    print(f"multilayer_perceptron_res {multilayer_perceptron_res}")
