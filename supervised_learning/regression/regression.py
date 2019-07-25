# Code for building a neural network regression function for the Forest Fires dataset and converting the model to IML


import pandas as pd
import tensorflow as tf
import numpy as np
import sklearn
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Activation
from sklearn import preprocessing, model_selection
from sklearn.feature_selection import f_regression, mutual_info_regression
from to_iml import h52iml


# Compute a rough piecewise approximation of log(x + 1) in the range [0, exp(7)]
def approx_log(x):

    ys = list(range(8))
    xs = [np.exp(y) - 1 for y in ys]
    ms = [1 / (xs[i+1] - xs[i]) for i in range(7)]
    cs = [ys[i] - (ms[i]*xs[i]) for i in range(7)]
    # print("Gradients:")
    # print(ms)
    # print("Intercepts:")
    # print(cs)

    if x <= xs[1]:
        y = (ms[0] * x) + cs[0]
    elif x <= xs[2]:
        y = (ms[1] * x) + cs[1]
    elif x <= xs[3]:
        y = (ms[2] * x) + cs[2] 
    elif x <= xs[4]:
        y = (ms[3] * x) + cs[3]
    elif x <= xs[5]:
        y = (ms[4] * x) + cs[4]
    elif x <= xs[6]:
        y = (ms[5] * x) + cs[5]
    else:
        y = (ms[6] * x) + cs[6]

    return y


# Run main function to create and save neural network
def run():

    # Import data
    df = pd.read_csv('forestfires.csv')

    # Convert months and days to numerical value
    months = {'jan':1, 'feb':2, 'mar':3, 'apr':4, 'may':5, 'jun':6, 'jul':7, 'aug':8, 'sep':9, 'oct':10, 'nov':11, 'dec':12}
    df.month = df.month.map(lambda x: np.sin((2 * np.pi * months[x] / 12) - (np.pi / 2)) + 1)
    days = {'mon':1, 'tue':2, 'wed':3, 'thu':4, 'fri':5, 'sat':6, 'sun':7}
    df.day = df.day.map(lambda x: np.sin((2 * np.pi * days[x] / 7) - (np.pi / 2)) + 1)

    # Remove outliers and apply piecewise linear approximation of log function to area variable
    df.drop(df[df.area > 100].index, inplace=True)
    df.area = df.area.map(lambda x: approx_log(x))

    # Scale data to have zero mean and unit variance, printing differences and minimums for later use
    names = df.columns
    scaler = preprocessing.MinMaxScaler()
    maxes = df.max(axis=0)
    mins = df.min(axis=0)
    diffs =  maxes - mins
    print("Scaling factor:")
    print(diffs)
    print("Additive factor:")
    print(mins)
    df = scaler.fit_transform(df)
    df = pd.DataFrame(df, columns=names)

    # cv = df.cov()
    # print(cv['area'])
    # _, ps = f_regression(df.loc[:, 'X':'rain'], df.loc[:, 'area'])
    # s = sklearn.feature_selection.SelectKBest(mutual_info_regression, k=6)
    # s.fit(df.loc[:, 'X':'rain'], df.loc[:, 'area'])
    # i = s.get_support(indices=True)
    # print([df.columns[j] for j in i])
    # print(df.columns)
    # print(ps)
    # print([df.columns[i] for i in range(len(ps)) if ps[i] > 0.1])

    # Split data into test and train sets and recursively remove correlated/uniformative features
    X_train, X_test, y_train, y_test = model_selection.train_test_split(df.loc[:, 'X':'rain'], df.loc[:, 'area'], test_size=0.2)
    corr = X_train.corr()
    to_drop = set([])
    for n in corr.columns:
        if n in to_drop:
            continue
        else:
            to_drop.update([m for m in corr.columns if corr.loc[m, n] > 0.8 and m != n])
    mi = mutual_info_regression(X_train, y_train, random_state=0)
    to_drop.update([names[i] for i in range(len(mi)) if mi[i] <= 0.01])
    print("Variables to remove:")
    print([name for name in to_drop])
    X_train.drop(to_drop, axis=1, inplace=True)
    X_test.drop(to_drop, axis=1, inplace=True)
    print("Variables left:")
    print([X_train.columns[i] for i in range(len(X_train.columns))])

    # Partition data and remove input variables with zero mutual information w.r.t. the output variable
    # X_train, X_test, y_train, y_test = model_selection.train_test_split(df.loc[:, 'X':'rain'], df.loc[:, 'area'], test_size=0.2)
    # mi = mutual_info_regression(X_train, y_train, random_state=0)
    # print(mi)
    # to_drop = [names[i] for i in range(len(mi)) if mi[i] == 0.0]
    # print("Variables to remove:")
    # print(to_drop)
    # X_train.drop(to_drop, axis=1, inplace=True)
    # X_test.drop(to_drop, axis=1, inplace=True)

    # Create network
    model = Sequential()
    model.add(Dense(6, activation='relu', input_dim=len(X_train.columns)))
    # model.add(Dense(3, activation='relu'))
    model.add(Dense(1, activation='linear'))

    # Train and test model
    model.compile(optimizer='sgd', loss='mean_squared_error')
    model.fit(X_train, y_train, epochs=250, batch_size=5)
    model.evaluate(X_test, y_test)

    # Save model
    model.save("forestfires.h5")
    

# Create a IML file representing a given neural network
def nn2iml(nn):

    # Intialise variables
    activation_functions = {'relu':     "let relu x = Real.(if x > 0.0 then x else 0.0);;",\
                            'linear':   "let linear x = Real.(x)"}
    layer_count = 0
    activations = set([])
    layers = []
    model_function = "let nn "
    
    # Create IML string for each layer in the network
    for layer in nn.layers:

        # Skip if there are no weights in the layer
        config = layer.get_config()
        weights = layer.get_weights()
        if len(weights) == 0:
            continue
        
        # Otherwise get layer information and continue
        else:
            in_size = np.prod(layer.input_shape[1:])
            input_vars = ["x_{}".format(i) for i in range(in_size)]
            out_size = np.prod(layer.output_shape[1:])
            output_vars = ["y_{}".format(i) for i in range(out_size)]

            # If first layer then add inputs to model string
            if layer_count == 0:
                model_function += "(" + ", ".join(input_vars) + ") = let open Real in \n("
                model_function += ", ".join(input_vars) + ")\n"

            # Record activation function
            activation_function = config['activation']
            activations.add(activation_function)

            # Create layer function
            layer_function = "let layer_{} (".format(layer_count) + ", ".join(input_vars) + ") = let open Real in\n"
            for j in range(out_size):
                w = " + ".join(["({:.5f})*{}".format(weights[0][i][j],input_vars[i]) for i in range(in_size)])
                if config['use_bias']:
                    b = " + {:.5f}".format(weights[1][j])
                else:
                    b = ""
                layer_function += "let y_{} = {} @@ {}{} in\n".format(j,activation_function,w,b)
            layer_function += "(" + ", ".join(output_vars) + ");;"

            # Add layer function and update model string
            layers.append(layer_function)
            model_function += "|> layer_{}\n".format(layer_count)
            layer_count += 1

    # Save model as IML file
    with open("forestfires.iml", 'w') as f:
        for a in activations:
            f.write(activation_functions[a] + "\n\n")
        for l in layers:
            f.write(l + "\n\n")
        f.write(model_function + ";;")


if __name__ == "__main__":

    # run()
    # nn = tf.keras.models.load_model("forestfires.h5", custom_objects={'tf': tf})
    # nn2iml(nn)

    # run('pre-processed-forestfires.csv')
    # h52iml('forestfires.h5')

    # Test model
    model = tf.keras.models.load_model('forestfires.h5', custom_objects={'tf': tf})
    x = np.reshape(np.array([(0.75, 0.198316675434, 0.79255685734, 0.79421221865, 0.188235294118, 0.)]), (1,6))
    print(np.shape(x))
    print(model.predict(x))