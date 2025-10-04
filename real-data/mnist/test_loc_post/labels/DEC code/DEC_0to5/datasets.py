import numpy as np


def load_mnist():
    x = np.load("0to5_selected_x_100.npy")
    x = x.reshape((x.shape[0], -1))
    x = np.divide(x, 255.)
    y = np.load("0to5_selected_y_100.npy")
    return x, y




def load_data(dataset_name):
    if dataset_name == 'mnist':
        return load_mnist()
    else:
        print('Not defined for loading', dataset_name)
        exit(0)