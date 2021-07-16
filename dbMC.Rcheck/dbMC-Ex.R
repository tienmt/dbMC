pkgname <- "dbMC"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "dbMC-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('dbMC')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("dbmc")
### * dbmc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dbmc
### Title: de-biased estimator
### Aliases: dbmc

### ** Examples


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
mean((x.db - xtrue)^2);rankMatrix(x.db,.1)[1]








base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dbmc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
