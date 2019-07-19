import numpy as np
import pandas as pd
from sklearn import preprocessing, model_selection
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import mutual_info_classif
import pickle

def run(file):

    # Import data and remove id column
    df = pd.read_csv(file, names = ['id', 'diagnosis', 'radius_mean', 'texture_mean', 'perimeter_mean',\
                                    'area_mean', 'smoothness_mean', 'compactness_mean', 'concavity_mean',\
                                    'concave points_mean', 'symmetry_mean', 'fractal_dimension_mean',\
                                    'radius_se', 'texture_se', 'perimeter_se', 'area_se', 'smoothness_se',\
                                    'compactness_se', 'concavity_se', 'concave points_se', 'symmetry_se',\
                                    'fractal_dimension_se', 'radius_worst', 'texture_worst',\
                                    'perimeter_worst', 'area_worst', 'smoothness_worst',\
                                    'compactness_worst', 'concavity_worst', 'concave points_worst',\
                                    'symmetry_worst', 'fractal_dimension_worst'], header = None)
    df.drop(['id'], axis=1, inplace=True)

    # Normalise data to have zero mean and unit variance
    data = df.loc[:, 'radius_mean':'fractal_dimension_worst']
    names = data.columns
    scaler = preprocessing.StandardScaler()
    means = data.mean(axis=0)
    stds = data.std(axis=0)
    print("Scaling factor:")
    print(stds)
    print("Additive factor:")
    print(means)
    data = scaler.fit_transform(data)
    data = pd.DataFrame(data, columns=names)
    df.loc[:, 'radius_mean':'fractal_dimension_worst'] = data

    # Split data into test and train sets and recursively remove worst features
    X_train, X_test, y_train, y_test = model_selection.train_test_split(df.loc[:, 'radius_mean':'fractal_dimension_worst'], df.loc[:, 'diagnosis'], test_size=0.2)
    mi = mutual_info_classif(X_train, y_train, random_state=0)
    print(mi)
    to_drop = [names[i] for i in range(len(mi)) if mi[i] <= 0.25]
    print("Variables to remove:")
    print(to_drop)
    X_train.drop(to_drop, axis=1, inplace=True)
    X_test.drop(to_drop, axis=1, inplace=True)

    print(len(X_train.columns))

    cv = X_train.cov()
    print(cv)

    

    # Create, fit, and save decision tree
    dt = DecisionTreeClassifier(max_depth=None, min_samples_split=2, min_samples_leaf=1, max_features=None, max_leaf_nodes=None)
    dt.fit(X_train, y_train)
    pickle.dump(dt, open("dt.pickle", 'wb'))


    # Create, fit, and save random forest
    rf = RandomForestClassifier(n_estimators=10, max_depth=None, min_samples_split=2, min_samples_leaf=1, max_features='auto', max_leaf_nodes=None)
    rf.fit(X_train, y_train)
    pickle.dump(dt, open("rf.pickle", 'wb'))

    # Check accuracy
    dt_acc = dt.score(X_test, y_test)
    rf_acc = rf.score(X_test, y_test)
    print("DT Accuracy: {:.3f}".format(dt_acc))
    print("RF Accuracy: {:.3f}".format(rf_acc))


if __name__ == "__main__":

    run('wdbc.csv')