import numpy as np
from time import time
from sklearn import metrics
import sys
sys.path.insert(0, "./code/py")
from get_change_point.get_fnn import get_fnn_model
from get_change_point.get_cnn import get_vgg16_model, get_vgg19_model
from misc.misc_v1 import get_ari
from misc.misc_v1 import get_cusum


def get_trained_clf(sample_, n, p, classifier="FNN", split_trim=0.15, n_layers_dense=128):
    k = int(np.floor(n * split_trim))
    tr_ind = np.concatenate((np.arange(k), np.arange((n - k), n)), axis=0)
    x_train = sample_[tr_ind]
    y_train = np.concatenate((np.zeros(k), np.ones(k)), axis=0)
    x_test = sample_[np.arange(k, n-k)]

    if classifier.upper() == "FNN":
        model = get_fnn_model(p)
    elif classifier.upper() == "VGG16":
        model = get_vgg16_model(shape=p, n_layers_dense=n_layers_dense)
    elif classifier.upper() == "VGG19":
        model = get_vgg19_model(shape=p, n_layers_dense=n_layers_dense)
    elif classifier.upper() == "VGG16_BW":
        model = get_vgg16_model(shape=p, n_layers_dense=n_layers_dense)
    else:
        pass
    model.fit(x_train, y_train, epochs=32, batch_size=32, verbose=0)
    pred = model.predict(x_test)[:, 0]

    return pred


def calculate_G_alpha_pval(observed_stat, B=10000, n_B=500, zeta=0.05, seed=None):


    if seed is not None:
        np.random.seed(seed)

    count = 0
    for _ in range(B):
        dt = 1.0 / n_B
        z = np.zeros(n_B + 1)
        for i in range(1, n_B + 1):
            z[i] = z[i - 1] + np.sqrt(dt) * np.random.randn()
        z = z[1:]

        r_start = int(np.floor(zeta * n_B))
        r_end = int(np.floor((1 - zeta) * n_B))
        r_range = np.arange(r_start, r_end)

        G = np.zeros_like(r_range, dtype=float)
        for idx, r in enumerate(r_range):
            r_n = r / n_B
            denom = np.sqrt(r_n * (1 - r_n))
            if denom > 0:
                G[idx] = (z[r] - r_n * z[-1]) / denom
            else:
                G[idx] = 0.0

        if np.max(G) >= observed_stat:
            count += 1

    pval = (count + 1) / (B + 1)
    return pval


def get_change_point(sample, classifier="FNN",
                     split_trim=0.15,
                     auc_trim=0.05,
                     use_simulated_pval=False,
                     sim_B=10000,
                     sim_n_B=500,
                     sim_zeta=0.05,
                     tau=0.5,
                     require_cusum=False,
                     n_layers_dense=128,
                     seed=None):
    st_time = time()

    if len(sample.shape) > 2:
        n = sample.shape[0]
        p = sample.shape[1:]
    else:
        n, p = sample.shape

    k = int(np.floor(n * split_trim))
    x_test = sample[np.arange(k, n-k)]
    nte = x_test.shape[0]
    start_ = int(np.floor(auc_trim * n))
    end_ = nte - int(np.floor(auc_trim * n))
    auc_ = np.zeros(nte - 2 * start_)

    pred = get_trained_clf(sample, n, p, classifier, split_trim, n_layers_dense=n_layers_dense)

    for i, j in enumerate(np.arange(start_, end_)):
        y_test_ = np.concatenate((np.zeros(j), np.ones(nte - j)), axis=0)
        auc_[i] = metrics.roc_auc_score(y_test_, pred)

    if require_cusum:
        cusum_ = get_cusum(pred=pred, n=n, auc_trim=auc_trim)
        max_cusum_ = np.max(cusum_)

    ch_pt_ = k + start_ + np.argmax(auc_)
    max_auc_ = np.max(auc_)
    ari_ = get_ari(n, int(np.floor(tau * n)), ch_pt_)

    out_dict = {
        "auc": auc_,
        "max_auc": max_auc_,
        "ch_pt": ch_pt_,
        "ari": ari_,
        "pred": pred
    }

    if require_cusum:
        out_dict["cusum"] = cusum_
        out_dict["max_cusum"] = max_cusum_

        if use_simulated_pval:
            print("Simulating Brownian bridge sup-G distribution for p-value...")
            pval_cusum_ = calculate_G_alpha_pval(
                observed_stat=max_cusum_,
                B=sim_B,
                n_B=sim_n_B,
                zeta=split_trim+auc_trim,
                seed=seed
            )
            out_dict["pval_cusum"] = pval_cusum_
            print(f"max_cusum_ = {max_cusum_:.4f}, p-value = {pval_cusum_:.4g}")

    en_time = time() - st_time
    out_dict["runtime"] = en_time
    print(f"Change point is detected at {ch_pt_} in {en_time:.2f} seconds")

    return out_dict
    #return pval_cusum_
