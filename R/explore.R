#' Univariate Exploration
#'
#' @param i column name from `x` to be used as a predictor
#' @param x data frame with Moose data
#' @param dist count distribution (P, NB, ZIP, or ZINB)
#'
#' @export
## density plots
# used to be plotUnivariateExpl
# x: MooseData
# i: colid
mc_plot_univariate <- function(i, x, dist="ZINB") {

    srv <- x$srv
    opts <- getOption("moose_options")
    d <- stats::density(x[,i])
    d_srv <- stats::density(x[srv,i])
    d_uns <- stats::density(x[!srv,i])

    ylim <- c(0, max(max(d_srv$y), max(d_uns$y)))
    xlim <- c(0, max(max(d_srv$x), max(d_uns$x)))

    z <- x[,i]
    xy <- x[, opts$xy]
    Col <- rev(grDevices::heat.colors(10))
    dat <- data.frame(y=x[srv,opts$Ntot], z=x[srv,i])
    dat <- dat[order(dat$z),]
    m <- zeroinfl2(y ~ z | 1, dat, dist=dist, link='logit')
    dat$zhat <- stats::fitted(m)

    op <- graphics::par(mfrow=c(1,3))
    plot(d, xlim=xlim,
      ylim=ylim + c(0, 0.2*diff(ylim)), xlab=i, main="Density")
    graphics::lines(d_srv, col=2)
    graphics::rug(x[,i], col=1, side=1)
    graphics::rug(x[srv,i], col=2, side=3)
    #graphics::lines(d_uns, col=4)
    graphics::legend("topleft", bty="n", lty=1, col=c(1,2),
        legend=c("All","Surveyed"))

    plot(xy, pch=19, col=Col[cut(z, 10)], main="Map",
        xlab="Longitude", ylab="Latitude",
        ylim=c(min(xy[,2])-0.2*diff(range(xy[,2])), max(xy[,2])))
    graphics::legend("bottomleft", bty="n", pch=19, col=Col[c(length(Col),1)],
      legend=c("High", "Low"))

    plot(jitter(y) ~ z, dat, main="Total moose response",
        xlab=i, ylab="Total moose", col="darkgrey", pch=19)
    graphics::lines(zhat ~ z, dat, col=4, lwd=2)

    graphics::par(op)
    invisible(NULL)
}

#' Multivariate Exploration
#'
#' @param vars column names from `x` to be used as a predictor
#' @param x data frame with Moose data
#'
#' @export
mc_plot_multivariate <- function(vars, x) {
    opts <- getOption("moose_options")
    z <- data.frame(Y=x[[opts$Ntot]], x[,vars])[x$srv,]
    tr <- partykit::ctree(Y ~ ., z,
        control = partykit::ctree_control(
            mincriterion = 1 - opts$alpha))
    plot(tr)
}

