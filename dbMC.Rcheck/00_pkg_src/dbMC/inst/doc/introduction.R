## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dbMC)
# simulated data
require(softImpute)
n = 100
p = 100
J = 2  # the true low-rank 
np = n*p
sig2 = 1
missfrac = 0.5
# xtrue is the underlying matrix that we do not know and want to recover it
xtrue = matrix(rnorm(n*J),n,J)%*%matrix(rnorm(J*p),J,p) 
# generating missing entries locations
imiss = sample(np,np*missfrac,replace=FALSE)
# xna is the observed matrix with missing entries
xna = xtrue + matrix(rnorm(np, sd = sig2),nr = n,nc = p)
xna[imiss] = NA
lamda = 2.5*sig2*sqrt(n*p)

# note that we only have xna as our initial data
# first, fit a softImpute method
fit1 = softImpute(xna, type = 'als')
# complete the matrix by a softImpute method
ximp = complete(xna,fit1)
mean((ximp - xtrue)^2);rankMatrix(ximp,.1)[1]
# now, de-biased the softImpute method
x.db = dbmc(x = xna,
            ximp = ximp,
            entries_miss = imiss,
            est_rank = 2)
# smaller mse with de-biased estimator
mean((x.db - xtrue)^2);rankMatrix(x.db,.1)[1]

# confidence intervals
CI_mc(i=1,j=2,alpha = 0.05,X.db = x.db,missfrac = 0.5,est_rank = 2,sigma2 = 1)
# true value
xtrue[1,2]


