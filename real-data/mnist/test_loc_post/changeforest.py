from changeforest import changeforest
import numpy as np

##scenario (i)
x = np.load("0_selected_x_150.npy")
x = x.reshape((x.shape[0], -1))
x = np.divide(x, 255.)
y = np.load("0_selected_y_150.npy")

CF_result = changeforest(x, "random_forest", "bs")
print(CF_result.p_value)


##scenario (ii)
x = np.load("383_selected_x_60_and_30.npy")
x = x.reshape((x.shape[0], -1))
x = np.divide(x, 255.)
y = np.load("383_selected_y_60_and_30.npy")

CF_result = changeforest(x, "random_forest", "bs")
print(CF_result.p_value)


##scenario (iii)
x = np.load("123_selected_x_60_and_30.npy")
x = x.reshape((x.shape[0], -1))
x = np.divide(x, 255.)
y = np.load("123_selected_y_60_and_30.npy")

CF_result = changeforest(x, "random_forest", "bs")
print(CF_result.p_value)



##scenario (iv)
x = np.load("0to5_selected_x_100.npy")
x = x.reshape((x.shape[0], -1))
x = np.divide(x, 255.)
y = np.load("0to5_selected_y_100.npy")

CF_result = changeforest(x, "random_forest", "bs")
print(CF_result.p_value)
