import numpy as np
from changeforest import changeforest

alpha = 0.1
NN = 1000

rng = np.random.default_rng(seed=11)

p_list = [5, 10]
n_list = [50, 100, 200, 400]

results = []

for p in p_list:
    for n in n_list:
        Sigma1 = np.eye(p)

        if p == 5:
            Sigma3 = np.eye(p)
            rho = 0.8
            block = np.fromfunction(lambda i, j: rho ** np.abs(i - j), (2, 2), dtype=int)
            Sigma3[:2, :2] = 0.5 * np.eye(2) + block
            #print(Sigma3)
        elif p == 10:
            Sigma3 = np.eye(p)
            rho = 0.8
            block = np.fromfunction(lambda i, j: rho ** np.abs(i - j), (4, 4), dtype=int)
            Sigma3[:4, :4] = 0.5 * np.eye(4) + block
            #print(Sigma3)

        rejections = []

        for _ in range(NN):
            mvn_samples1 = rng.multivariate_normal(np.zeros(p), Sigma1, int(0.5 * n), method="cholesky")
            mvn_samples2 = rng.multivariate_normal(np.zeros(p), Sigma3, int(0.5 * n), method="cholesky")

            X = np.concatenate((mvn_samples1, mvn_samples2), axis=0)

            result = changeforest(X, "random_forest", "bs")
            p_value = result.p_value
            rejections.append(int(p_value < alpha))

        rejection_rate = np.mean(rejections)
        results.append((p, n, rejection_rate))
        print(f"p={p}, n={n}, Rejection rate={rejection_rate:.3f}")

