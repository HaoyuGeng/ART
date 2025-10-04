#' TUNE: Algorithm-Agnostic Inference after Changepoint Detection
#'
#' TUNE method for inference after changepoint detection.
#'
#' @param data A data matrix of dimension \eqn{n \times d} if \code{type = "mean"}, where each row represents an observation, and each column stands for a variable. A dataframe of dimension \eqn{n \times (d+1)} if \code{type = "regression"}, where each row represents an observation, the first column stands for response and others stand for variables.
#' @param cps A numeric vector, specifying detected changepoints.
#' @param h A numeric value, specifying bandwidth size \eqn{h}.
#' @param test.stat A character string specifying the test statistic. Currently \code{"Wald", "score", "dist", "compr", "SN"} are supported.
#' @param type A character string specifying the type of test statistic if \code{test.stat = "wald"} or \code{"score"}. Currently \code{"mean", "regrerssion"} are supported.
#' @param norm A character string specifying the type of \eqn{g(\cdot)\in \{\Vert\cdot\Vert_2,\Vert\cdot\Vert_{\infty}\}} only if \code{test.stat = "score"}. Currently, \code{"L2","Linf"} are supported.
#' @param B A numeric value only if \code{thres = NULL}, specifying the number of bootstrap replications if \code{test.stat = "score"}, the number of permutation replications if \code{test.stat = "dist"}, the number of simulation replications if \code{test.stat = "SN"} or \code{"compr"}. It is ignored if \code{thres} is specified or \code{test.stat = "Wald"}.
#' @param alpha A numeric value, specifying the significant level \eqn{\alpha} of test.
#' @param thres A numeric value, specifying the threshold level for testing whether a changepoint is a true changepoint. If \code{thres = NULL}, the threshold is determined by asymptotic distribution if \code{test.stat = "Wald"}, bootstrap if \code{test.stat = "score"}, permutation if \code{test.stat = "dist"} and simulation if \code{test.stat = "compr"} or \code{"SN"}.
#' @return \code{TUNE} returns a numeric vector containing the locations of identifying changepoints.
#' @export
#'
#'@references TUNE: Algorithm-Agnostic Inference after Changepoint Detection
#'
#' @examples
#'set.seed(500)
#'library("MASS")
#'library(cpss)
#'n = 500
#'d = 5
#'h = 20
#'Sigma = diag(d)
#'for(i in 1:d) {
#'  for(j in 1:d) {
#'    Sigma[i,j] = 0.5^{abs(i - j)}
#'  }
#'}
#'data = t(cbind(t(mvrnorm(n / 5, mu = rep(0, d), Sigma)),
#'               t(mvrnorm(n / 5, mu = rep(2, d), Sigma)),
#'               t(mvrnorm(n / 5, mu = rep(0, d), Sigma)),
#'               t(mvrnorm(n / 5, mu = rep(2, d), Sigma)),
#'               t(mvrnorm(n / 5, mu = rep(0, d), Sigma))))
#'cps = cpss.mean(data, algorithm = "BS", criterion = "CV", Sigma = Sigma)@cps
#'cps
#'# 97 200 250 300 402
#'TUNE(data = data,cps = cps,h = h,
#'     test.stat = "Wald",type = "mean",alpha=0.05,thres=NULL)
#'# 97 200 300 402
#'
TUNE = function(data, cps, h,
                test.stat = c("Wald", "score", "dist", "compr", "SN")[1],
                type = c("mean", "regression")[1],
                norm = c("L2", "Linf")[2], B=200, alpha=0.05, thres=NULL) {
  d = dim(data)[2]
  if(test.stat == "SN") {
    if(d > 1) {
      stop("test.stat is not avaliable!")
    }else {
      results = TUNE_SN_uni(data = data, cps = cps, h =h,
                            thres = thres, B = B, alpha = alpha)
      return(results)
    }
  }else if(test.stat == "Wald") {
    results = TUNE_Wald(data = data, cps = cps, h = h,
                        type = type, thres_Wald = thres,
                        alpha = alpha)
    return(results)
  }else if(test.stat == "score") {
    results= TUNE_score(data = data, cps = cps, h = h,
                        norm = norm, type = type,
                        B = B, alpha = alpha,
                        thres_score = thres)
    return(results)
  }else if(test.stat == "dist") {
    results = TUNE_dist(data = data, cps = cps, h=h,
                        thres_dist = thres, B = B,alpha = alpha)
    return(results)
  }else if (test.stat == "compr") {
    results = TUNE_compr(data = data, cps= cps,h =h,
                         thres_compr = thres,B= B,alpha = alpha)
    return(results)
  }else{
    stop("test.stat is not avaliable!")
  }
}
