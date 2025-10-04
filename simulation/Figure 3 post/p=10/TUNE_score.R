####### HIGH DIMENSIONAL CASE
### 3.1.3 & 3.1.4 TUNE_score
TUNE_score_mean_threshold_L2 = function(data, h, B=200, alpha) {
  data = t(data)
  n = dim(data)[2]
  p = dim(data)[1]
  x = matrix(stats::rnorm(n * B),nrow = B)
  boot_statistics = rep(0, B)
  for(i in 1:B) {
    tt = NULL
    for(j in h:(n - h)) {
      left = j - h + 1
      right = j + h
      vy = sqrt(sum(abs(rowMeans(t(x[i,left:j] * t(data[,(left + 1):(j + 1)] - data[,left:j])))-
                          rowMeans(t(x[i,(j + 1):right] * t(data[,(j + 1):right] - data[,j:(right - 1)]))))^2))
      vy = vy / sqrt(2)
      vy = vy / (sqrt((1 / (j - left + 1) + (1 / (right - j)))))
      tt = c(tt, vy)
    }
    boot_statistics[i] = max(tt)
  }
  return(list(thres = stats::quantile(boot_statistics, 1 - alpha),
              alpha = alpha,
              emp_thresholds = boot_statistics))}
TUNE_score_mean_threshold_Linf = function(data, h, B=200, alpha) {
  data = t(data)
  n = dim(data)[2]
  p = dim(data)[1]
  x = matrix(stats::rnorm(n * B),nrow = B)
  boot_statistics = rep(0, B)
  for(i in 1:B) {
    tt = NULL
    for(j in h:(n - h)) {
      left = j - h + 1
      right = j + h
      vy = max(abs(rowMeans(t(x[i,left:j] * t(data[,(left + 1):(j + 1)]-data[,left:j])))-
                     rowMeans(t(x[i,(j + 1):right]*t(data[,(j + 1):right]-data[,j:(right - 1)])))))
      vy = vy / sqrt(2)
      vy = vy / (sqrt((1 / (j - left + 1) + (1 / (right - j)))))
      tt = c(tt, vy)
    }
    boot_statistics[i] = max(tt)

  }
  return(list(thres = stats::quantile(boot_statistics, 1 - alpha),
              alpha = alpha,
              emp_thresholds = boot_statistics))}
TUNE_score_mean = function(data, cps, h,
                           norm = c("L2","Linf")[1],
                           B=200, alpha=0.05,
                           thres_score = NULL) {
  if(norm == "L2") {
    if(is.null(thres_score)) {
      thres_score = TUNE_score_mean_threshold_L2(data, h, B, alpha)$thres
    }
    data = t(data)
    results = NULL
    n = dim(data)[2]
    for(cp in cps) {
      if(cp < h | cp > (n - h)) {
        results = c(results, FALSE)
      }else{
        left = cp - h + 1
        right = cp + h
        vy = sqrt(sum(abs(rowMeans(data[,left:cp])-rowMeans(data[,(cp + 1):right]))^2))
        vy = vy / (sqrt((1 / (cp - left + 1) + (1 / (right - cp)))))
        results = c(results, vy > thres_score)
      }
    }
    return(cps[results])
  }else if(norm == "Linf") {
    if(is.null(thres_score)) {
      thres_score = TUNE_score_mean_threshold_Linf(data, h, B, alpha)$thres
    }
    data = t(data)
    results = NULL
    n = dim(data)[2]
    for(cp in cps) {
      if(cp < h | cp > (n - h)) {
        results = c(results, FALSE)
      }else {
        left = cp - h + 1
        right = cp + h
        vy = max(abs(rowMeans(data[,left:cp]) - rowMeans(data[,(cp + 1):right])))
        vy = vy / (sqrt((1 / (cp - left + 1) + (1 / (right - cp)))))
        results = c(results, vy > thres_score)
      }
    }
    return(cps[results])
  }else{
    stop("norm is not avaliable!")
  }
}
yxprod = function(data_use) {
  y = data_use[, 1]
  x = data_use[, -1]
  data_z = y * x
  return(data_z)
}
TUNE_score_regression_threshold = function(data_score, n, h,
                                           norm = c("L2", "Linf")[2],
                                           alpha=0.05, B=200) {

  p = ncol(data_score)
  boot_vec = rep(NA, B)
  if (norm == "L2") {
    for (i in 1:B) {
      xi = stats::rnorm(n, 0, 1)
      stat_boot_once = rep(NA, (n - 2 * h + 1))
      for (l in h:(n - h)) {
        xi_bef = xi[(l - h + 1):l]
        xi_aft = xi[(l + 1):(l + h)]
        var_bef = apply(data_score[(l - h + 1):(l + 1), ], 2, diff)
        var_aft = apply(data_score[l:(l + h), ], 2, diff)
        data_boot_bef = apply(var_bef, 2, function(x) x * xi_bef)
        data_boot_aft = apply(var_aft, 2, function(x) x * xi_aft)
        data_boot_bef=data_boot_bef/sqrt(2)
        data_boot_aft=data_boot_aft/sqrt(2)
        stat_boot_single_vec = (colSums(data_boot_bef) - colSums(data_boot_aft))/(sqrt(2 * h))
        stat_boot_once[l - h + 1] = sqrt(sum(stat_boot_single_vec^2))
      }
      boot_vec[i] = max(stat_boot_once)
    }
  }
  if(norm == "Linf") {
    for(i in 1:B) {
      xi = stats::rnorm(n, 0, 1)
      stat_boot_once = rep(NA, (n - 2 * h + 1))
      for(l in h:(n - h)) {
        xi_bef = xi[(l - h + 1):l]
        xi_aft = xi[(l + 1):(l + h)]
        var_bef = apply(data_score[(l - h + 1):(l + 1), ], 2, diff)
        var_aft = apply(data_score[l:(l + h), ], 2, diff)
        data_boot_bef = apply(var_bef, 2, function(x) x * xi_bef)
        data_boot_aft = apply(var_aft, 2, function(x) x * xi_aft)
        data_boot_bef=data_boot_bef/sqrt(2)
        data_boot_aft=data_boot_aft/sqrt(2)
        stat_boot_single_vec = (colSums(data_boot_bef) - colSums(data_boot_aft))/(sqrt(2 * h))
        stat_boot_once[l - h + 1] = max(abs(stat_boot_single_vec))
      }
      boot_vec[i] = max(stat_boot_once)
    }
  }
  return(list(thres = stats::quantile(boot_vec, 1 - alpha), emp_thresholds = boot_vec))
}
TUNE_score_regression = function(data, cps, h,
                                 norm=c("L2","Linf")[1],
                                 B=200, alpha=0.05,
                                 thres_score=NULL) {
  n = dim(data)[1]
  if(!(norm %in% c("L2","Linf"))) {
    stop("norm is not avaliable!")
  }
  #compute score function
  data_score = yxprod(data_use = data)
  if(is.null(thres_score)) {
    #compute threshold by bootstrapping
    thres_score = TUNE_score_regression_threshold(data_score = data_score,
                                                  alpha = alpha, n = n, h = h, B = B,
                                                  norm = norm)$thres
  }
  #compute test statistics for each changepoints
  n_cps = length(cps)
  stat_max = rep(NA, n_cps)
  for (i in 1:n_cps) {
    tau_est = cps[i]
    if(tau_est < h | tau_est > (n-h)) {
      stat_max[i] = 0
    }else{
      coef_bef = colSums(data_score[(tau_est - h + 1):tau_est, ])/sqrt(2 * h)
      coef_aft = colSums(data_score[(tau_est + 1):(tau_est + h), ])/sqrt(2 * h)
      if (norm == "L2") {
        stat_max[i] = sqrt(sum((coef_bef - coef_aft)^2))
      }
      if (norm == "Linf") {
        stat_max[i] = max(abs(coef_bef - coef_aft))
      }
    }
  }
  results = stat_max>thres_score
  return(cps[results])
}

TUNE_score = function(data, cps, h,
                      norm=c("L2", "Linf")[1], type=c("mean", "regression")[1],
                      B=200, alpha=0.05,
                      thres_score=NULL) {
  if(type == "mean") {
    results = TUNE_score_mean(data = data, cps = cps, h = h, norm = norm,
                              B = B, alpha = alpha, thres_score = thres_score)
    return(results)
  }else if (type == "regression") {
    results = TUNE_score_regression(data = data, cps = cps, h = h,
                                    norm = norm, alpha = alpha, B = B, thres_score = thres_score)
    return(results)
  }else{
    stop("type is not avaliable!")
  }
}

