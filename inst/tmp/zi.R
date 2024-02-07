source("R/models.R")
# library(moosecounter)
solvenear <- moosecounter:::solvenear

## ZI Poisson

n <- 2000
phi <- 0.7
beta <- c(1,0.5)
x <- rnorm(n)
X <- model.matrix(~x)
mu <- drop(X %*% beta)
z <- rbinom(n, 1, 1-phi)
Z <- matrix(1, n, 1)

Y <- rpois(n, exp(mu) * z)

m1 <- zeroinfl2(Y ~ x | 1, dist="ZIP")
m2 <- zeroinfl2(Y ~ x | 1, dist="ZIP", robust=TRUE)
m3 <- .zeroinfl_clpl(Y, X, Z, distr="ZIP")

cbind(zi2=coef(m1),
    zi2robust=coef(m2),
    clpl=m3$par)
cbind(zi2=sqrt(diag(vcov(m1))),
    zi2robust=sqrt(diag(vcov(m2))),
    clpl=sqrt(diag(solve(-m3$hessian))))



## Poisson
Y <- rpois(n, exp(mu))

m1 <- zeroinfl2(Y ~ x | 1, dist="P")
m2 <- zeroinfl2(Y ~ x | 1, dist="P", robust=TRUE)
m3 <- .zeroinfl_clpl(Y, X, Z, distr="P")

cbind(zi2=coef(m1),
    zi2robust=coef(m2),
    clpl=c(m3$par, NA))
cbind(zi2=sqrt(diag(vcov(m1))),
    zi2robust=sqrt(diag(vcov(m2))),
    clpl=c(sqrt(diag(solve(-m3$hessian))),NA))


## ZI NegBin

n <- 10000
phi <- 0.7
beta <- c(1,0.5)
x <- rnorm(n)
X <- model.matrix(~x)
mu <- drop(X %*% beta)
z <- rbinom(n, 1, 1-phi)
Z <- matrix(1, n, 1)
theta <- 0.1

lambda <- drop(exp(X %*% beta))
A <- rbinom(n, 1, 1-phi)
Y <- stats:::rnbinom(n, size=theta, mu=lambda*A)

m1 <- zeroinfl2(Y ~ x | 1, dist="ZINB")
m2 <- zeroinfl2(Y ~ x | 1, dist="ZINB", robust=TRUE)
m3 <- .zeroinfl_clpl(Y, X, Z, distr="ZINB")

coef(m1)
coef(m2)
sqrt(diag(vcov(m1)))
sqrt(diag(vcov(m2)))
log(m1$theta)
log(m2$theta)
m3$par


## NegBin
Y <- stats:::rnbinom(n, size=theta, mu=lambda)

m1 <- zeroinfl2(Y ~ x | 1, dist="NB")
m2 <- zeroinfl2(Y ~ x | 1, dist="NB", robust=TRUE)
m3 <- .zeroinfl_clpl(Y, X, Z, distr="NB")

coef(m1)
coef(m2)
sqrt(diag(vcov(m1)))
sqrt(diag(vcov(m2)))
log(m1$theta)
log(m2$theta)
m3$par

# hurdle

devtools::install()
library(moosecounter)

n <- 200
phi <- 0.7
beta <- c(1,0.5)
x <- rnorm(n)
X <- model.matrix(~x)
mu <- drop(X %*% beta)
z <- rbinom(n, 1, 1-phi)
Z <- matrix(1, n, 1)

Y <- rpois(n, exp(mu) * z)


mh <- zeroinfl2(Y ~ x | 1, dist="ZIP", hurdle = TRUE)



library(moosecounter)

mc_options(B=20)
x <- read.csv("inst/extdata/MayoMMU_QuerriedData.csv")
switch_response("total")
x <- mc_update_total(x)

vars <- c("ELC_Subalpine", "Fire1982_2012", "Fire8212_DEM815",
    "NALC_Needle", "NALC_Shrub", "Subalp_Shrub_250buf",
    "ELCSub_Fire8212DEM815", "SubShrub250_Fire8212DEM815")

xv <- FALSE
hurdle <- TRUE

ML <- list()
ML[["Model 0"]] <- mc_fit_total(x, dist="ZINB", xv=xv, hurdle=hurdle)
ML[["Model 1"]] <- mc_fit_total(x, vars[1:2], dist="ZINB", xv=xv, hurdle=hurdle)
ML[["Model 2"]] <- mc_fit_total(x, vars[2:3], dist="ZIP", xv=xv, hurdle=hurdle)
ML[["Model 3"]] <- mc_fit_total(x, vars[3:4], dist="ZINB", xv=xv, hurdle=hurdle)

PI <- mc_predict_total(
    model_id=c("Model 1", "Model 3"),
    ml=ML,
    x=x,
    do_boot=T, do_avg=TRUE)

