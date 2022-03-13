#' Set Moose Options
#'
#' A function to get and set the package related options.
#'
#' * `method`: Optimization algorithm
#' * `response`: Response variable
#' * `MAXCELL`: Max total abundance in cells
#' * `MINCELL`: Min abundance for composition in cells
#' * `B`: Number of bootstrap runs
#' * `alpha`: Type I error rate for PI
#' * `wscale`: Weight
#' * `sightability`: Sightability
#' * `seed`: Random seed
#' * `Ntot`: set to `MOOSE_TOTA` (internally changing to `"COW_TOTA"` when modeling cows)
#' * `srv_name`: Filtering variable
#' * `srv_value`: Filtering value
#' * `area_srv`: Comun indicating survey areas
#' * `Area`: Area
#' * `xy`: Long/Lat
#' * `composition`: Composition variables
#'
#' @param ... options to set (see Details)
#'
#' @examples
#' ## original values
#' o <- mc_options()
#' str(o)
#' o$B
#'
#' ## set B to new value
#' mc_options(B = 20)
#' mc_options()$B
#'
#' ## restore orgiginal values
#' mc_options(o)
#' mc_options()$B
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
