#' Random Numbers From ZINB Distribution
#'
#' ZINB = zero inflated negative binomial.
#'
#' @param N the number of random numbers to produce
#' @param mu.nb the mean of the NB distribution
#' @param theta.nb the dispersion parameter of the NB distribution, a single nonnegative numeric value or `NULL` (this refers to no overdispersion, thus a Poisson or ZIP distribution)
#' @param phi.zi the probability of the non-zero valies in the ZI part
#' @return A numeric vector of length `N` with random numbers
#' @examples
#' table(rZINB(100, 3, 2, 0.5))
#' table(rZINB(100, 3, NULL, 0.5))
# note: phi.zi is prob of 1 (not 0 as in zeroinfl)
# theta.nb=NULL gives poisson
#' @export
rZINB <- function(N, mu.nb, theta.nb, phi.zi) {
    if (length(phi.zi) < N)
        phi.zi <- rep(phi.zi, N)[seq_len(N)]
    A <- stats::rbinom(N, 1, phi.zi)
    if (!is.null(theta.nb)) {
        Z <- MASS::rnegbin(N, mu=mu.nb, theta=rep(theta.nb, N))
    } else {
        Z <- stats::rpois(N, lambda=mu.nb)
    }
    Y <- A * Z
    Y
}

#' Robust Matrix Inversion
#'
#' @param x a symmetric square matrix
#'
#' @export
solvenear <- function (x) {
    if (is.null(x))
        return(NULL)
    xinv <- try(solve(x), silent = TRUE)
    if (inherits(xinv, "try-error"))
        xinv <- as.matrix(solve(Matrix::nearPD(x)$mat))
    xinv
}

#' Switch Response
#'
#' @param type type of the response, can be `"total"` or `"cows"`
#'
#' @export
switch_response <- function(type="total") {
    type <- match.arg(type, c("total", "cows"))
    opts <- getOption("moose_options")
    if (type == "total") {
        opts$Ntot <- "MOOSE_TOTA"
        opts$composition <- c("BULL_SMALL", "BULL_LARGE", "LONE_COW",
            "COW_1C", "COW_2C", "LONE_CALF", "UNKNOWN_AG")
    }
    if (type == "cows") {
        opts$Ntot <- "COW_TOTA"
        opts$composition <- c("LONE_COW", "COW_1C", "COW_2C")
    }
    opts$response <- type
    mc_options(opts)
}

#' Find Mode
#'
#' @param x a numeric vector
#'
#' @export
## need to use density to get the mode because values tend to
## be spread out
find_mode <- function(x) {
    d <- stats::density(x)
    round(d$x[which.max(d$y)])
}
