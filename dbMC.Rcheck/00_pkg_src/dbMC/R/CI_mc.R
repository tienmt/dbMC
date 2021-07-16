#' compute the confidence intervals (CIs) from the de-biased estimator
#' 
#' @description 
#' This function returns a CI for an entries of interest with a significant level alpha
#' 
#' @param i the row index of the interest entry X_{i,j}
#' @param j the row index of the interest entry X_{i,j}
#' @param alpha confidence level, default is 0.05
#' @param missfrac the missing proportion in the underlying matrix. 
#' It is the total of missing entries over total entries.
#' @param X.db the de-biased estimated matrix from the 'dbmc' function.
#' @param est_rank the (estimated) low-rank of the underlying matrix or the rank of the de-biased estimator.
#' @param sigma2 the noise-variance level.
#' 
#' @return CI confidence interval.
#' @return (i,j) the location of the entry at i-th row and j-th column.
#' @return v.ij the estimated variance of the limiting Gaussian distribution.
#' 
#' @references Chen et al (2019). Inference and uncertainty quantification for noisy matrix completion. PNAS, 116(46), 22931-22937.
#' 
#' @export
CI_mc = function(i,j,
                   alpha = 0.05,
                   missfrac,
                   X.db,
                   est_rank,
                   sigma2 = 1){
  tam = svd(X.db,nu =est_rank,nv = est_rank)
  n = nrow(X.db)
  p = ncol(X.db)
  lamda = 2.5*sigma2*sqrt(n*p)
  Xd = tam$u[,1:est_rank] %*% diag( sqrt(tam$d[1:est_rank] + lamda) )
  Yd = tam$v[,1:est_rank]%*% diag( sqrt(tam$d[1:est_rank] + lamda) )
  v.ij = sigma2*( Xd[i,] %*%solve(crossprod(Xd))%*% Xd[i,] + Yd[j,] %*%solve(crossprod(Yd))%*% Yd[j,])/(1-missfrac)
  ci = x.db[i,j] + c(-1,1)* stats::qnorm(1-alpha/2)*sqrt(c(v.ij) )
  return(list('CI' = ci,'(i,j)' = c(i,j),v.ij = v.ij))
}
