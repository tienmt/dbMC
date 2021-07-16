#' de-biased estimator
#' @description de-biased low-rank matrix completion estimator
#'
#' This function compute a de-biased estimator from a rank-r matrix completion using 
#' the algorithms from the package "softImpute".
#' 
#' @param x the initial matrix with missing entries
#' @param ximp an imputed matrix, output from the package "softImpute".
#' @param entries_miss the missing indices
#' @param est_rank the rank of the matrix x, or the estimated rank from the package "softImpute".
#' 
#' @return x.db the de-baised estimation matrix.
#' @references Chen et al (2019). Inference and uncertainty quantification for noisy matrix completion. PNAS, 116(46), 22931-22937.
#' 
#' @example R/example.R
#' 
#' @export
dbmc = function(x,
                ximp,
                entries_miss,
                est_rank){
  tam = ximp- P_Omega(ximp - x,entries_miss)
  tam = svd(tam)
  x.db = tam$u[,1:est_rank] %*% diag(tam$d[1:est_rank]) %*% t(tam$v[,1:est_rank])
  return(x.db)
}
