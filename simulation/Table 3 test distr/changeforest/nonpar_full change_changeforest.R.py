import numpy as np
from changeforest import changeforest

rng = np.random.default_rng(11)

p_list = [5, 10]
n_list = [50, 100, 200, 400]
alpha = 0.1
NN = 1000
results = []

for p in p_list:
    for n in n_list:
        Sigma1 = np.eye(p)
        rho = 0.8
        Sigma2 = np.fromfunction(lambda i, j: rho ** np.abs(i - j), (p, p), dtype=float)
        #print(Sigma2)

        rejections = []

        for _ in range(NN):
            mvn_samples1 = rng.multivariate_normal(np.zeros(p), Sigma1, int(0.5*n), method="cholesky")
            mvn_samples2 = rng.multivariate_normal(np.zeros(p), 0.5*Sigma1 + Sigma2, int(0.5*n), method="cholesky")
            X = np.concatenate((mvn_samples1, mvn_samples2), axis=0)
            result = changeforest(X, "random_forest", "bs")
            p_value = result.p_value

            rejections.append(int(p_value < alpha))

        rejection_rate = np.mean(rejections)
        results.append((p, n, rejection_rate))
        print(f"p={p}, n={n}, Rejection rate={rejection_rate:.4f}")
