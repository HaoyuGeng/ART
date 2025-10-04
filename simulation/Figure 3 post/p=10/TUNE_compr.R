### 3.1.5 rank based TUNE (compr)
# compute statistic U
compute_U_h = function(data, tau, h) {
  n = nrow(data)
  d = ncol(data)
  U_h_tau = numeric(d)
  for (k in 1:d) {
    sum_term = 0
    for (i in (tau - h + 1):tau) {
      for (j in (tau + 1):(tau + h)) {
        sum_term = sum_term + (as.numeric(data[i, k] <= data[j, k]) - as.numeric(data[j, k] <= data[i, k]))
      }
    }
    U_h_tau[k] = (1 / sqrt(2 * h^3)) * sum_term
  }
  return(U_h_tau)
}
# compute the empirical distribution
compute_empirical_cdf = function(data, tau, h) {
  n = nrow(data)
  d = ncol(data)
  empirical_cdf = matrix(0, nrow = 2 * h, ncol = d)
  for (k in 1:d) {
    for (i in (tau - h + 1):(tau + h)) {
      empirical_cdf[i - (tau - h), k] = mean(data[(tau - h + 1):(tau + h), k] <= data[i, k])
    }
  }
  return(empirical_cdf)
}
# compute the covariance
compute_covariance_matrix = function(data, tau, h, empirical_cdf) {
  d = ncol(data)
  n = nrow(data)
  covariance_matrix = matrix(0, nrow = d, ncol = d)
  for (k in 1:d) {
    for (k_prime in 1:d) {
      sum_term = 0
      for (i in (tau - h + 1):(tau + h)) {
        F_h_k = empirical_cdf[i - (tau - h), k]
        F_h_k_prime = empirical_cdf[i - (tau - h), k_prime]
        sum_term = sum_term + (F_h_k - 0.5) * (F_h_k_prime - 0.5)
      }
      covariance_matrix[k, k_prime] = (2 / h) * sum_term
    }
  }
  if(d > n) {
    covariance_matrix = diag(diag(covariance_matrix))
  }
  return(covariance_matrix)
}
# compute the test statistic
compute_T_tau = function(U_h_tau, covariance_matrix) {
  T_tau = t(U_h_tau) %*% solve(covariance_matrix) %*% U_h_tau
  return(T_tau)
}
# compute the maximum of statistic
compute_max_T_tau = function(data, h) {
  n = nrow(data)
  max_T_tau = -Inf
  for (tau in h:(n - h)) {
    U_h_tau = compute_U_h(data, tau, h)
    empirical_cdf = compute_empirical_cdf(data, tau, h)
    covariance_matrix = compute_covariance_matrix(data, tau, h, empirical_cdf)
    T_tau = compute_T_tau(U_h_tau, covariance_matrix)
    if (T_tau > max_T_tau) {
      max_T_tau = T_tau
    }
  }
  return(max_T_tau)
}
# compute test statistic for all changepoints of interest
compute_stat_compr = function(data, cps, h) {
  T_tau_vector = numeric(length(cps))
  n = dim(data)[1]
  for (i in 1:length(cps)) {
    cp = cps[i]
    if(cp < h | cp > (n-h)) {
      T_tau_vector[i] = 0
    }else{
      U_h_tau = compute_U_h(data, cp, h)
      empirical_cdf = compute_empirical_cdf(data, cp, h)
      covariance_matrix = compute_covariance_matrix(data, cp, h, empirical_cdf)
      T_tau_vector[i] = compute_T_tau(U_h_tau, covariance_matrix)
    }
  }
  return(T_tau_vector)
}
# compute threshold
TUNE_compr_threshold = function(n, d, h, B=200, alpha=0.05) {
  max_statistics = numeric(B)
  for (i in 1:B) {
    data = MASS::mvrnorm(n, mu = rep(0, d), Sigma = diag(d))
    max_T_tau = compute_max_T_tau(data, h)
    max_statistics[i] = max_T_tau
  }
  return(list(thres = stats::quantile(max_statistics,1-alpha),
              alpha = alpha,
              emp_thresholds = max_statistics))
}
TUNE_compr=function(data, cps, h, thres_compr = NULL, B=200, alpha=0.05) {
  n=dim(data)[1]
  d=dim(data)[2]
  if(is.null(thres_compr)) {
    thres_compr = TUNE_compr_threshold(n, d, h, B, alpha)$thres
  }
  stat_compr = compute_stat_compr(data, cps, h)
  results = NULL
  for(i in 1:length(cps)) {
    results = c(results, stat_compr[i] > thres_compr)
  }
  return(cps[results])
}
