import numpy as np
from changeforest import changeforest

# NULL
p = 10 #10
n = 1000  #50 100 200
Sigma1 = np.diag([1] * p)
Sigma2 = np.full((p, p), 0.9)
np.fill_diagonal(Sigma2, 1)

rng = np.random.default_rng(11)

alpha = 0.1
NN = 1000

rejections = []
for _ in range(NN):
    X = np.concatenate(
        (
            rng.multivariate_normal(np.zeros(p), Sigma1, int(0.5*n), method="cholesky"),
            rng.multivariate_normal(np.zeros(p), Sigma1, int(0.5*n), method="cholesky")
        ),
        axis=0
    )

    result = changeforest(X, "random_forest", "bs")

    p_value = result.p_value
    #print(p_value)
    if p_value < alpha:
        rejections.append(1)
    else:
        rejections.append(0)

rejection_rate = np.mean(rejections)
print(f"Rejection average: {rejection_rate}")


