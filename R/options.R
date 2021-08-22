#' Set Moose Options
#'
#' @param ... options to set (see `zzz.R`)
#'
#' @export
## define global options
mc_options <- function(...) {
    opar <- getOption("moose_options")
    args <- list(...)
    if (length(args)) {
        if (length(args)==1 && is.list(args[[1]])) {
            npar <- args[[1]]
        } else {
            npar <- opar
            npar[match(names(args), names(npar))] <- args
        }
        options("moose_options"=npar)
    }
    invisible(opar)
}
