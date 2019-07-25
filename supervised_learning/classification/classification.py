# Code for building a random forest classifier for the Wisconsin Breast Cancer (Diagnostic) dataset and converting the model to IML


import numpy as np
import pandas as pd
from sklearn import preprocessing, model_selection
from sklearn.tree import DecisionTreeClassifier, export_text
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import mutual_info_classif
import pickle
from sklearn.tree import _tree


# Run main function to create and save random forest and decision tree
def run():

    # Import data and remove id column
    df = pd.read_csv('wdbc.csv', names = ['id', 'diagnosis', 'radius_mean', 'texture_mean', 'perimeter_mean',\
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

    # Split data into test and train sets and recursively remove correlated/uniformative features
    X_train, X_test, y_train, y_test = model_selection.train_test_split(df.loc[:, 'radius_mean':'fractal_dimension_worst'], df.loc[:, 'diagnosis'], test_size=0.2)
    corr = X_train.corr()
    to_drop = set([])
    for n in corr.columns:
        if n in to_drop:
            continue
        else:
            to_drop.update([m for m in corr.columns if corr.loc[m, n] > 0.9 and m != n])    
    mi = mutual_info_classif(X_train, y_train, random_state=0)
    to_drop.update([names[i] for i in range(len(mi)) if mi[i] <= 0.2])
    print("Variables to remove:")
    print([name for name in to_drop])
    X_train.drop(to_drop, axis=1, inplace=True)
    X_test.drop(to_drop, axis=1, inplace=True)
    print("Variables left:")
    print([X_train.columns[i] for i in range(len(X_train.columns))])

    # Create, fit, and save decision tree
    dt = DecisionTreeClassifier(max_depth=3, min_samples_split=2, min_samples_leaf=4, max_features=None, max_leaf_nodes=None)
    dt.fit(X_train, y_train)
    pickle.dump(dt, open("dt.pickle", 'wb'))

    # Create, fit, and save random forest
    rf = RandomForestClassifier(n_estimators=3, max_depth=3, min_samples_split=2, min_samples_leaf=4, max_features='auto', max_leaf_nodes=None)
    rf.fit(X_train, y_train)
    pickle.dump(rf, open("rf.pickle", 'wb'))

    # Check accuracy
    dt_acc = dt.score(X_train, y_train)
    rf_acc = rf.score(X_train, y_train)
    print("DT Train Accuracy: {:.3f}".format(dt_acc))
    print("RF Train Accuracy: {:.3f}".format(rf_acc))
    dt_acc = dt.score(X_test, y_test)
    rf_acc = rf.score(X_test, y_test)
    print("DT Test Accuracy: {:.3f}".format(dt_acc))
    print("RF Test Accuracy: {:.3f}".format(rf_acc))


# Create a IML string representing a given decision tree
def dt2iml(dt, num, names):

    # Initialise variables
    tree_ = dt.tree_
    feature_name = [names[i] for i in tree_.feature]
    dt_s = "let tree_{} {} = let open Real in".format(num, " ".join(names))

    # Recurse over tree structure to complete string
    def recurse(dt_s, node, depth):
        indent = "  " * depth
        if tree_.feature[node] != _tree.TREE_UNDEFINED:
            name = feature_name[node]
            threshold = tree_.threshold[node]
            dt_s += "\n{}if {} <=. ({:.5f}) then".format(indent, name, threshold)
            dt_s = recurse(dt_s, tree_.children_left[node], depth + 1)
            dt_s += "\n{}else".format(indent)
            dt_s = recurse(dt_s, tree_.children_right[node], depth + 1)
        else:
            dt_s += "\n{}({}, {})".format(indent, tree_.value[node][0][0], tree_.value[node][0][1])
        return dt_s  
           
    return recurse(dt_s, 0, 1) + ";;" 


# Create a IML file representing a given random forest
def rf2iml(rf):

    # Intialise variables
    names = ["f_{}".format(i) for i in range(rf.n_features_)]
    rf_s = "let rf ({}) = let open Real in".format(", ".join(names))
    trees = []
    num_trees = 0

    # Add each decision tree in the ensemble
    for dt in rf.estimators_:
        trees.append(dt2iml(dt, num_trees, names))
        rf_s += "\nlet (a_{0}, b_{0}) = tree_{0} {1} in".format(num_trees, " ".join(names))
        num_trees += 1

    # Add random forest as function
    rf_s += "\nlet a = {} in".format(" + ".join(["a_{}".format(i) for i in range(num_trees)]))
    rf_s += "\nlet b = {} in".format(" + ".join(["b_{}".format(i) for i in range(num_trees)]))
    rf_s += "\n(a, b);;"

    # Write output to IML file
    with open('wdbc.iml', 'w') as f:
        for t in trees:
            f.write(t + "\n\n")
        f.write(rf_s)

    
if __name__ == "__main__":

    run()
    with open('rf.pickle', 'rb') as f: 
        rf = pickle.load(f)
    rf2iml(rf)