import os
import pickle as pkl
import random
import sys
import numpy as np

from get_change_point.get_multiple_change_point_v1 import get_multiple_change_point
from get_change_point.get_change_point_v1 import get_change_point


with open("heatmaps_color_numeric.pkl", "rb") as f:
    heatmaps_array = pkl.load(f)

random.seed(100)


out = get_change_point(heatmaps_array, classifier="VGG16", split_trim=0.15, auc_trim=0.05,require_cusum=True,use_simulated_pval=True)
print(out)


