#####functions#####
main_multi_test_fun <- function(score_all,inter_l){
  library(MASS)
  library(glmnet)
  
  n <- length(score_all)
  ###--- score ---
  rank_score_all <- rank(score_all, ties.method = "random")
  
  if(n<=300){
    inter_l = 0.2*n 
    inter_center = c(0.3,0.4,0.5,0.6,0.7)*n  
  }else{
    inter_center = (3:(floor(n/(inter_l/2))-3) / floor(n/(inter_l/2))) * n
  }
  
  Sn <- NULL
  for (ii_inter in 1:length(inter_center)) {
    ind_low <- inter_center[ii_inter] - inter_l/2 +1
    ind_up <- inter_center[ii_inter] + inter_l/2
    n_l <- inter_l
    rank_score <- rank_score_all[ind_low:ind_up]
    rank_score <- rank(rank_score,ties.method = "random")
    
    Sn_t_all <- max(
      sapply((0.1*n_l):(0.9*n_l), function(i){
        abs(sum(rank_score[1:i]) - i*mean(rank_score))
      })
    )
    Sn <- c(Sn, Sn_t_all)
  }
  Sn <- max(Sn)
  
  #threshold for rejection
  B=1000
  Sn_B <- rep(0,B)
  
  for (bb in 1:B) {
    
    rank_bb_all <-  rank(runif((n),0,1), ties.method = "random")
    Sn_bb <- NULL
    for (ii_inter in 1:length(inter_center)) {
      ind_low <- inter_center[ii_inter] - inter_l/2 +1
      ind_up <- inter_center[ii_inter] + inter_l/2
      n_l <- inter_l
      rank_bb = rank_bb_all[ind_low:ind_up]
      rank_bb <- rank(rank_bb,ties.method = "random")
      Sn_bb_all <- max(
        sapply((0.1*n_l):(0.9*n_l), function(i){
          abs(sum(rank_bb[1:i]) - i*mean(rank_bb))
        })
      )
      Sn_bb <- c(Sn_bb, Sn_bb_all)
    }
    Sn_B[bb] <- max(Sn_bb)
  }
  
  p_value <- (sum(Sn < Sn_B) + runif(1,0,1) * (sum((Sn == Sn_B)+0)+1) ) / (B+1)
 
  result_list <- list(p_value = p_value)
  return(result_list)
}








