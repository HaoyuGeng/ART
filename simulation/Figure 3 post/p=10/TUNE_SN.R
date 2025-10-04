
### 3.2.2 self-normalization based TUNE
# compute C_n
calculate_C_n = function(X) {
  n = length(X)
  X_mean = mean(X)
  C_n = numeric(n)
  cumulative_sum = 0
  for (j in 1:n) {
    cumulative_sum = cumulative_sum + (X[j] - X_mean)
    C_n[j] = cumulative_sum / sqrt(n)
  }
  return(C_n)
}
# compute L_n(k | s, e)
calculate_L_n = function(C_n, s, k, e, n) {
  term1 = C_n[k] - ifelse(s > 1, C_n[s - 1], 0)
  term2 = (k - s + 1) / (e - s + 1) * (C_n[e] - ifelse(s > 1, C_n[s - 1], 0))
  L_n_k = sqrt(n / (e - s + 1)) * (term1 - term2)
  return(L_n_k)
}
# compute V_n(k | s, e)
calculate_V_n = function(C_n, s, k, e, n) {
  L_n_values_s_k = sapply(s:k, function(j) calculate_L_n(C_n, s, j, k, n))
  L_n_values_k1_e = sapply((k+1):e,function(j) calculate_L_n(C_n, k + 1, j, e, n))
  V_n_k = ((k - s + 1) / ((e - s + 1)^2)) * sum(L_n_values_s_k^2)+
    ((e-k) / ((e-s+1)^2)) * sum(L_n_values_k1_e^2)
  return(V_n_k)
}
# compute T(k | s, e)
calculate_T = function(C_n, s, k, e, n) {
  L_n_k = calculate_L_n(C_n, s, k, e, n)
  V_n_k = calculate_V_n(C_n, s, k, e, n)
  T_k = (L_n_k^2) / V_n_k
  return(T_k)
}
# compute the maximum of T
calculate_max_T = function(X, h) {
  n = length(X)
  C_n = calculate_C_n(X)
  T_values = numeric(n - 2 * h + 1)
  for (tau in h:(n - h)) {
    s = tau - h + 1
    k = tau
    e = tau + h
    T_values[tau - h + 1] = calculate_T(C_n, s, k, e, n)
  }
  max_T_value = max(T_values)
  return(max_T_value)
}
# compute test statistics
calculate_stat_SN = function(X, cps, h) {
  n = length(X)
  C_n = calculate_C_n(X)
  cps_cand=!(cps < h | cps > (n-h) )
  stat_cps=rep(0,length(cps))
  T_values = sapply(cps[cps_cand], function(tau) {
    s = tau - h + 1
    k = tau
    e = tau + h
    calculate_T(C_n, s, k, e, n)
  })
  stat_cps[cps_cand]=T_values
  return(stat_cps)
}
TUNE_SN_uni_threshold = function(n, h, B=200, alpha=0.05) {
  max_statistics = numeric(B)
  for (b in 1:B) {
    X = stats::rnorm(n)
    max_statistics[b] = calculate_max_T(X, h)
  }
  quantile_value = stats::quantile(max_statistics, 1 - alpha)
  return(list(thres = stats::quantile(max_statistics, 1 - alpha),
              alpha = alpha,
              emp_thresholds = max_statistics))
}
TUNE_SN_uni = function(data, cps, h, thres=NULL, B=200, alpha=0.05) {
  stat_SN = calculate_stat_SN(data, cps, h)
  n=length(data)
  if(is.null(thres)) {
    thres = TUNE_SN_uni_threshold(n, h, B, alpha)$thres
  }
  results = stat_SN > thres
  return(cps[results])
}
