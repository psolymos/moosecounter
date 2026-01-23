# Simulation functions

## RSF covariates

#' Simulate spatial RSF
#'
#' @param dim_x,dim_y X and Y dimension for the grid.
#' @param phi Parameter for the spatial correlation decay function.
#'
#' @export
sim_rsf <- function(dim_x = 50, dim_y = 50, phi = 0.05) {
    rmvn <- function(n, mu = 0, V = matrix(1)) {
        p <- length(mu)
        D <- chol(V)
        unname(t(matrix(rnorm(n * p), ncol = p) %*% D + rep(mu, rep(n, p))))
    }
    xv <- seq_len(dim_x)
    yv <- seq_len(dim_y)
    m <- expand.grid(x = xv, y = yv)
    n <- nrow(m)
    d <- as.matrix(dist(m))
    m$z <- drop(rmvn(1, rep(0, n), exp(-phi * d)))
    data.frame(m)
}

#' Simulate expected value
#'
#' @param sim_rsf Data frame returned by [sim_rsf()].
#' @param intercept,slope Intercept and slope.
#' @param intercept0,slope0 Intercept and slope for ZI.
#'
#' @export
sim_expected <- function(
    sim_rsf,
    intercept = 0,
    slope = 0,
    intercept1 = 0,
    slope1 = 0
) {
    sim_rsf$mu <- exp(intercept + slope * sim_rsf$z)
    sim_rsf$phi1 <- plogis(intercept1 + slope1 * sim_rsf$z)
    sim_rsf
}

#' Simulate counts
#'
#' @param sim_exp Data frame returned by [sim_expected()].
#' @param theta.nb dispersion parameter for NB.
#' @param type ZI or Hurdle model.
#'
#' @export
sim_counts <- function(
    sim_exp,
    theta.nb = NULL,
    type = c("none", "zeroinfl", "hurdle")
) {
    N <- nrow(sim_exp)
    type <- match.arg(type)
    if (type == "none") {
        y <- rZINB(
            N,
            mu.nb = sim_exp$mu,
            theta.nb = theta.nb,
            phi.zi = 1
        )
    }
    if (type == "zeroinfl") {
        y <- rZINB(
            N,
            mu.nb = sim_exp$mu,
            theta.nb = theta.nb,
            phi.zi = sim_exp$phi1
        )
    }
    if (type == "hurdle") {
        y <- rHurdle(
            N,
            mu.nb = sim_exp$mu,
            theta.nb = theta.nb,
            phi.zi = sim_exp$phi1
        )
    }
    sim_exp$MOOSE_TOTA <- y
    sim_exp
}

#' Simulate stratification
#'
#' @param sim_rsf Data frame returned by [sim_rsf()].
#' @param threshold Threshold for low/high.
#' @param p_misclass Misclassification probability.
#'
#' @export
sim_strat <- function(sim_rsf, threshold = 0, p_misclass = 0) {
    sim_rsf$stratum <- ifelse(sim_rsf$z >= threshold, 1, 0)
    k <- rbinom(nrow(sim_rsf), 1, p_misclass)
    sim_rsf$stratum[k > 0] <- 1 - sim_rsf$stratum[k > 0]
    sim_rsf
}
