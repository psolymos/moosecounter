#' Total Estimate with Gassaway Formula
#'
#' We do the stratification BEFORE the surveys are done using the
#' fixed wing aircraft. Every cell is stratified as LOW or HIGH.
#' Then some of the LOW and some of HIGH are surveyed to find how
#' many moose are there.
#'
#' @param y1,y2 surveyed cell-level population counts in the 2 strata
#' @param N1,N2 total number of cells that were stratified
#' @param x object to print
#' @param ... arguments passed to the print method
#'
#' @examples
#' y1 <- rpois(20, 50)
#' y2 <- rpois(30, 5)
#' N1 <- 40
#' N2 <- 50
#' mc_gassaway(y1, y2, N1, N2)
#'
#' @name gassaway
NULL

# Gassaway formula
#
# We do the stratification BEFORE the surveys are done using the fixed wing aircraft. Every cell is stratified as LOW or HIGH. And, then some of the LOW and some of HIGH are surveyed to find how many moose are there. The average of the moose number in the LOW stratum cells that were surveyed is mu_1 and average of the HIGH is mu_2.
#
# This is the notation:
#    L = 2 (two strata; Low and High).
#    mu_i is the mean of the i-th stratum (mean number of Moose in Low and mean number of moose in High).
#    s2_i = variance of the ith stratum (variance of the number of Moose in the Low stratum and variance of the number of Moose in the High stratum samples. This should use the denominator (n-1), not n.)
#    N_1 and N_2 are the total number of cells that were stratified as LOW and HIGH after the strat flight.
#    n_1 and n_2 are the number of cells that were sampled in LOW and HIGH strata.
#' @rdname gassaway
#' @export
mc_gassaway <- function(y1, y2, N1, N2) {

  ## address R CMD notes related to NSE
  Stratum <- counts <- strata <- N <- NULL

  df <- data.frame(
    counts=c(y1, y2),
    Stratum=factor(
      c(rep("Stratum 1", length(y1)),
      rep("Stratum 2", length(y2))),
      c("Stratum 1", "Stratum 2")))
  stats <- df %>%
    dplyr::group_by(Stratum) %>%
    dplyr::summarise(
      Mean=mean(counts),
      SD=stats::sd(counts), # sd() uses small sample corrected N-1 denom
      n=length(counts)) %>%
    as.data.frame()
  stats$N <- c(N1, N2)
  tot <- with(stats, Mean * N)
  Var <- with(stats, N^2 * ((N-n)/N) / (SD^2/n))
  tau_hat <- sum(tot)
  var_hat <- sum(Var)
  out <- list(counts=counts, strata=strata, N=N,
       stats=stats,
       total=tau_hat, variance=var_hat)
  class(out) <- "mc_gassaway"
  out
}

#' @rdname gassaway
#' @export
print.mc_gassaway <- function(x, ...) {
  cat("Total Moose population estimate - Gassaway formula\n\n",
      "Total = ", format(x$total, ...),
      " (Variance = ", format(x$variance, ...), ")\n\n", sep="")
  print(x$stats, ...)
  invisible(x)
}
