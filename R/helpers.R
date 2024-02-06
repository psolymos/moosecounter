#' Internal Functions: Helpers
#'
#' Exported functions intended to be used internally.
#'
#' `ZINB` makes zero inflated negative binomial random numbers.
#'
#' `solvenear` inverts near-PD matrices.
#' It function makes sure that near-positive definite matrices
#' are positive definite. Positive definiteness is needed
#' for matrix inversion, which in turn is used to find
#' the Hessian matrix and standard errors for model coefficients
#' from numerical optimization.
#'
#' The `find_mode` function finds the mode of a distribution using
#' one-dimensional kernel density estimation.
#' The density based estimate is rounded, because
#' the function is used in the context of count data models
#' and predictions.
#'
#' @param N The number of random numbers to produce.
#' @param mu.nb The mean of the NB distribution.
#' @param theta.nb The dispersion parameter of the NB distribution,
#'   a single non-negative numeric value or `NULL` (this refers to
#'   no overdispersion, thus a Poisson or ZIP distribution).
#' @param phi.zi The probability of the non-zero valies in the ZI part.
#' @param x A symmetric square matrix for `solvenear`,
#'   a numeric vector for `find_mode`.
#'
#' @examples
#' table(rZINB(100, 3, 2, 0.5))
#' table(rZINB(100, 3, NULL, 0.5))
#'
#' x <- c(1, 2, 1, 3, 4, 3, 2, 3, 5, 6, 10)
#' find_mode(x)
#' plot(density(x))
#' rug(x)
#' abline(v = find_mode(x), lty=2)
#'
#' @keywords internal
#' @name internal
NULL

# note: phi.zi is prob of 1 (not 0 as in zeroinfl)
# theta.nb=NULL gives poisson
#' @rdname internal
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

#' @rdname internal
#' @export
solvenear <- function (x) {
    if (is.null(x))
        return(NULL)
    xinv <- try(solve(x), silent = TRUE)
    if (inherits(xinv, "try-error"))
        xinv <- as.matrix(solve(Matrix::nearPD(x)$mat))
    xinv
}

#' @rdname internal
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

#' @noRd
PI_xlslist <- function(file, pred, summary, seed, subset = "Full") {
  o <- mc_options()
  o <- append(o, c("random seed" = seed))

  info <- data.frame(moosecounter = paste0(
    c("R package version: ", "Date of analysis: ", "File: "),
    c(ver, format(Sys.time(), "%Y-%m-%d"), file$name)))

  settings <- data.frame(
    Option = names(o),
    Value = sapply(o, paste, sep = "", collapse = ", ")) %>%
    rbind(cbind(Option = "Subset", Value = subset))

  # Ensure measurements included in output
  summary <- as.data.frame(summary)
  summary <- cbind(measurement = rownames(summary), summary)

  list(
    Info = info,
    Settings = settings,
    Summary = summary,
    Data = pred$data,
    Boot = pred$boot_full)
}

