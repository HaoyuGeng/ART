### 3.1.5 distance-based TUNE (dist)
# compute threshold
TUNE_dist_threshold = function(data, h, B=200, alpha=0.05) {
  data = t(data)
  n = dim(data)[2]
  p = dim(data)[1]
  permu_B = rep(0, B)
  for(ii in 1:B) {
    data = data[, sample(1:n)]
    tt = NULL
    for(j in h:(n - h)) {
      left = j - h + 1
      right = j + h
      vy = 0
      for(i in left:j) {
        for(l in (j + 1):right) {
          vy = vy + sqrt(sum((data[,i] - data[,l])^2)) / (h^2/2)
        }
      }
      for(i in left:(j - 1)) {
        for(l in (i + 1):j) {
          vy = vy - sqrt(sum((data[,i] - data[,l])^2))/(h * (h - 1) / 2)
        }
      }
      for(i in (j + 1):(right - 1)) {
        for(l in (i + 1):right) {
          vy = vy - sqrt(sum((data[,i] - data[,l])^2)) / (h * (h - 1) / 2)
        }
      }
      tt=c(tt, abs(vy))
    }
    permu_B[ii] = max(tt)
  }
  return(list(thres = stats::quantile(permu_B, 1 - alpha),
              alpha = alpha,
              emp_thresholds = permu_B))
}
TUNE_dist = function(data, cps, h, thres_dist = NULL, B=200, alpha=0.05) {
  n=dim(data)[1]
  d=dim(data)[2]
  if(is.null(thres_dist)) {
    thres_dist = TUNE_dist_threshold(data, h, B, alpha=alpha)$thres
  }
  data = t(data)
  results = NULL
  for(cp in cps) {
    if(cp < h | cp > (n-h)) {
      results = c(results, FALSE)
    }else{
      left = cp-h+1
      right = cp + h
      j = cp
      vy = 0
      for(i in left:j) {
        for(l in (j + 1):right) {
          vy = vy + sqrt(sum((data[,i] - data[,l])^2)) / (h^2 / 2)
        }
      }
      for(i in left:(j - 1)) {
        for(l in (i + 1):j) {
          vy = vy - sqrt(sum((data[,i] - data[,l])^2)) / (h * (h - 1) / 2)
        }
      }
      for(i in (j + 1):(right - 1)) {
        for(l in (i + 1):right) {
          vy = vy - sqrt(sum((data[,i] - data[,l])^2)) / (h * (h - 1) / 2)
        }
      }
      vy = abs(vy)
      results=c(results, vy > thres_dist)
    }
  }
  return(cps[results])
}
