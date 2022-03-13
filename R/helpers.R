#' Internal Functions: Helpers
#'
#' `ZINB` makes zero inflated negative binomial random numbers.
#'
#' @param N the number of random numbers to produce
#' @param mu.nb the mean of the NB distribution
#' @param theta.nb the dispersion parameter of the NB distribution, a single non-negative numeric value or `NULL` (this refers to no overdispersion, thus a Poisson or ZIP distribution)
#' @param phi.zi the probability of the non-zero valies in the ZI part
#' @return A numeric vector of length `N` with random numbers
#' @examples
#' table(rZINB(100, 3, 2, 0.5))
#' table(rZINB(100, 3, NULL, 0.5))
#' @rdname internal
#' @keywords internal
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
#' `solvenear` inverts near-PD matrices.
#' It function makes sure that near-positive definite matrices
#' are positive definite. Positive definiteness is needed
#' for matrix inversion, which in turn is used to find
#' the Hessian matrix and standard errors for model coefficients
#' from numerical optimization.
#'
#' @param x a symmetric square matrix for `solvenear`,
#'   a numeric vector for `find_mode`
#'
#' @rdname internal
#' @keywords internal
#' @export
solvenear <- function (x) {
    if (is.null(x))
        return(NULL)
    xinv <- try(solve(x), silent = TRUE)
    if (inherits(xinv, "try-error"))
        xinv <- as.matrix(solve(Matrix::nearPD(x)$mat))
    xinv
}

#' Find Mode
#'
#' The `find_mode` function finds the mode of a distribution using
#' one-dimensional kernel density estimation.
#' The density based estimate is rounded, because
#' the function is used in the context of count data models
#' and predictions.
#'
#' @examples
#' x <- c(1, 2, 1, 3, 4, 3, 2, 3, 5, 6, 10)
#' find_mode(x)
#' plot(density(x))
#' rug(x)
#' abline(v = find_mode(x), lty=2)
#'
#' @rdname internal
#' @keywords internal
#' @export
## need to use density to get the mode because values tend to
## be spread out
find_mode <- function(x) {
    d <- stats::density(x)
    round(d$x[which.max(d$y)])
}

#' Pipe operator
#'
#' `\%>\%`: imported function,
#' see \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname internal
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
