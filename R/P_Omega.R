
#' projection onto observation set
#' 
#' @description 
#' This function returns a matrix where the missing entries are replaced by 0 s.
#' 
#' @param a a matrix.
#' @param entri missing entries location.
#' @return Return a matrix whose its missing entries are replaced by 0 s.
P_Omega = function(a,entri){
  a[entri] = 0
  return(a)
}
P_Omega <- compiler::cmpfun(P_Omega)


