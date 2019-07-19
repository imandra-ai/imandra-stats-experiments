from sys import argv
import pandas as pd
import tensorflow as tf
import numpy as np
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Activation


activation_functions = {'relu':     "let relu x = Real.(if x > 0.0 then x else 0.0);;",\
                        'linear':   "let linear x = Real.(x)"}

def rf_test():

    return

def nn_test():

    model = Sequential()
    model.add(Dense(10, activation='relu', input_dim=10))

    model.add(Dense(10, activation='relu'))
    model.add(Dense(10, activation='relu'))
    model.add(Dense(10, activation='relu'))
    model.add(Dense(10, activation='relu'))

    model.add(Dense(5, activation='relu'))

    model.compile(optimizer='rmsprop',
                loss='categorical_crossentropy',
                metrics=['accuracy'])

    data = np.random.random((1000, 10))
    labels = np.random.randint(5, size=(1000, 1))

    one_hot_labels = tf.keras.utils.to_categorical(labels, num_classes=5)

    model.fit(data, one_hot_labels, epochs=10, batch_size=32)

    model.save("small.h5")


def h52iml(h5_file):

    iml_file =  h5_file[:-3] + ".iml"
    model = tf.keras.models.load_model(h5_file, custom_objects={'tf': tf})
    layer_count = 0
    activations = set([])
    layers = []
    model_function = "let nn "
    
    for layer in model.layers:

        config = layer.get_config()
        weights = layer.get_weights()

        if len(weights) == 0:
            continue
        
        else:

            in_size = np.prod(layer.input_shape[1:])
            input_vars = ["x_{}".format(i) for i in range(in_size)]
            out_size = np.prod(layer.output_shape[1:])
            output_vars = ["y_{}".format(i) for i in range(out_size)]

            if layer_count == 0:
                model_function += " ".join(input_vars) + " = let open Real in \n("
                model_function += ", ".join(input_vars) + ")\n"

            activation_function = config['activation']
            activations.add(activation_function)

            layer_function = "let layer_{} (".format(layer_count) + ", ".join(input_vars) + ") = let open Real in\n"
            
            for j in range(out_size):

                w = " + ".join(["({:.5f})*{}".format(weights[0][i][j],input_vars[i]) for i in range(in_size)])
                if config['use_bias']:
                    b = " + {:.5f}".format(weights[1][j])
                else:
                    b = ""

                layer_function += "let y_{} = {} @@ {}{} in\n".format(j,activation_function,w,b)
            
            layer_function += "(" + ", ".join(output_vars) + ");;"
            layers.append(layer_function)
            model_function += "|> layer_{}\n".format(layer_count)
            layer_count += 1

    with open(iml_file, 'w') as f:

        for a in activations:
            f.write(activation_functions[a] + "\n\n")

        for l in layers:
            f.write(l + "\n\n")

        f.write(model_function + ";;")


# if __name__ == "__main__":

#     # test()

#     h5_file = argv[1]

#     h52iml(h5_file)