####### 3.1.1 Univariate mean change case
TUNE_Wald_uni = function(data, cps, h, thres,sd = 1) {
  results = NULL
  n = length(data)
  for(cp in cps) {
    if(cp < h | cp > (n - h)) {
      results = c(results, FALSE)
    }else{
      left = cp - h + 1
      right = cp + h
      vy = abs(mean(data[left:cp]) - mean(data[(cp + 1):right]))
      vy = vy / sd
      results = c(results, vy > thres)
    }
  }
  return(cps[results])
}
### 3.1.2 Wald based TUNE, mean change, regression coefficient change
## compute threshold
TUNE_Wald_threshold = function(n, h, d, alpha=0.05) {
  thres = sqrt(2 / h) * (2 * log(n / h) + d * 0.5 * log(log(n / h)) + log(1.5) - log(gamma(d / 2))
                     - log(-0.5 * log(1 - alpha))) / (
                       sqrt(2 * log(n / h)))
  return(thres)
}
TUNE_Wald_mean = function(data, cps, h, thres_Wald = NULL, alpha = 0.05, Sigma = NULL) {
  results = NULL
  data = t(data)
  n = dim(data)[2]
  d = dim(data)[1]
  if(is.null(thres_Wald)) {
    thres_Wald = TUNE_Wald_threshold(n, h, d, alpha)
  }
  for(cp in cps) {
    if(cp < h | cp > (n - h)) {
      results = c(results, FALSE)
    }else{
      left = cp - h + 1
      right = cp + h
      if(is.null(Sigma)) {
        hatsig_p = matrix(0, ncol=d, nrow=d)
        for(index_tau in 1:(n - 1)) {
          hatsig_p = hatsig_p +
            (data[, index_tau + 1]-data[, index_tau]) %*% t((data[, index_tau + 1] - data[, index_tau]))
        }
        hatsig_p = hatsig_p / (2 * (n - 1))
      }else{
        hatsig_p = Sigma
      }
      hat_sig_inv2 = pracma::sqrtm(hatsig_p)$Binv
      vy = sqrt(sum(abs(rowMeans(hat_sig_inv2%*%data[, left:cp]) - rowMeans(hat_sig_inv2%*%data[, (cp + 1):right]))^2))
      results = c(results, vy > thres_Wald)
    }
  }
  return(cps[results])
}
TUNE_Wald_regression = function(data, cps, h, thres_Wald=NULL, alpha=0.05) {
  n = dim(data)[1]
  d = dim(data)[2]
  n_cps = length(cps)
  Cov_X = stats::cov(data[,-1])
  results = rep(NA, n_cps)
  if(is.null(thres_Wald)) {
    thres_Wald = TUNE_Wald_threshold(n, h, d)
  }
  for(i in 1:n_cps) {
    cp = cps[i]
    if(cp < h | cp > (n - h)) {
      results[i] = FALSE
    }else{
      data_i = data[(cp - h + 1):(cp + h),]
      lm_i = stats::lm(y~.+0., data = data_i)
      res_i = lm_i$residuals
      coe_i = lm_i$coefficients
      diff_eps = 0
      for(ii in 2:(2 * h)) {
        diff_eps = diff_eps + (res_i[ii] - res_i[ii-1])^2
      }
      diff_eps = diff_eps / (2 * (2 * h - 1))
      Gamma_i_12_diff = pracma::sqrtm(Cov_X)$B / sqrt(diff_eps)
      theta_l = stats::lm(y~.+0., data=data_i[1:h,])$coefficients
      theta_r = stats::lm(y~.+0.,data=data_i[(h + 1):(2 * h),])$coefficients
      T_wald_i = sqrt(sum((Gamma_i_12_diff%*%matrix(theta_l-theta_r))^2))
      results[i] = T_wald_i > thres_Wald
    }
  }
  return(cps[results])

}
TUNE_Wald = function(data, cps, h, type = c("mean","regression"), thres_Wald = NULL, alpha=0.05) {
  n=dim(data)[1]
  if(is.null(thres_Wald)) {
    d = dim(data)[2]
    thres_Wald = TUNE_Wald_threshold(n, h, d)
    #thres_Wald = 0
  }
  if(type == "mean") {
    results = TUNE_Wald_mean(data, cps, h, thres_Wald)
    return(results)
  }else if(type == "regression") {
    results = TUNE_Wald_regression(data, cps, h, thres_Wald)
    return(results)
  }else{
    stop("type is not avaliable!")
  }
}






