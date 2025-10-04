import pickle as pkl
import numpy as np
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans
import math
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import random

random.seed(0)
np.random.seed(0)

with open("heatmaps_color_numeric.pkl", "rb") as f:
    heatmaps_array = pkl.load(f)  # shape: (1826, 32, 32, 3)

n_samples = heatmaps_array.shape[0]
flattened_array = heatmaps_array.reshape(n_samples, -1)  # shape: (1826, 3072)

pca = PCA(n_components=50, random_state=0)
pca_features = pca.fit_transform(flattened_array)  # shape: (1826, 50)

kmeans = KMeans(n_clusters=math.floor(7*1.5), random_state=0)
labels = kmeans.fit_predict(pca_features)  # shape: (1826,)


_, idx = np.unique(labels, return_index=True)
original_order = np.array(labels)[np.sort(idx)]
label_map = {old_label: new_label for new_label, old_label in enumerate(original_order)}

labels = np.array([label_map[l] for l in labels])



df = pd.DataFrame({'image_index': np.arange(len(labels)), 'cluster_label': labels})
df.to_csv('cluster_labels.csv', index=False)

print("saved in cluster_labels.csv")




