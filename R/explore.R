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

    opts <- getOption("moose_options")
    srv <- x$srv
    z <- x[,i]
    CAT <- is.factor(z) || is.character(z)
    if (CAT) {
        z <- droplevels(as.factor(z))
#        if (any(table(z, srv) == 0L))
#            stop("Categorical variable has different levels in surveyed vs. unsurveyed")
    }
    xy <- x[, opts$xy]
    dat <- data.frame(y=x[srv,opts$Ntot], z=z[srv])
    dat <- dat[order(dat$z),]
    m <- zeroinfl2(y ~ z | 1, dat, dist=dist, link='logit')
    dat$zhat <- stats::fitted(m)

    op <- graphics::par(mfrow=c(1,3))
    on.exit(graphics::par(op))

    if (CAT) {
        d <- table(z)/length(srv)
        d_srv <- table(z[srv])/sum(srv)
        d_uns <- table(z[!srv])/sum(!srv)
        ylim <- c(0, max(max(d_srv), max(d_uns)))

        graphics::barplot(rbind(d, d_srv), main="Density", beside=TRUE, col=1:2)
        graphics::legend("topleft", bty="n", fill=c(1,2),
            legend=c("All","Surveyed"))

        Col <- rev(grDevices::hcl.colors(length(d), "Set 2"))
        plot(xy, pch=19, col=Col[as.integer(z)], main="Map",
            xlab="Longitude", ylab="Latitude", asp=1,
            ylim=c(min(xy[,2])-0.2*diff(range(xy[,2])), max(xy[,2])))

        plot(jitter(y) ~ z, dat, main="Total moose response",
            xlab=i, ylab="Total moose", col="darkgrey", pch=19)
        graphics::lines(zhat ~ z, dat, col=4, lwd=2)

    } else {
        d <- stats::density(x[,i])
        d_srv <- stats::density(x[srv,i])
        d_uns <- stats::density(x[!srv,i])
        ylim <- c(0, max(max(d_srv$y), max(d_uns$y)))
        xlim <- c(0, max(max(d_srv$x), max(d_uns$x)))

        plot(d, xlim=xlim,
          ylim=ylim + c(0, 0.2*diff(ylim)), xlab=i, main="Density")
        graphics::lines(d_srv, col=2)
        graphics::rug(x[,i], col=1, side=1)
        graphics::rug(x[srv,i], col=2, side=3)
        #graphics::lines(d_uns, col=4)
        graphics::legend("topleft", bty="n", lty=1, col=c(1,2),
            legend=c("All","Surveyed"))

        Col <- rev(grDevices::heat.colors(10))
        plot(xy, pch=19, col=Col[cut(z, 10)], main="Map",
            xlab="Longitude", ylab="Latitude", asp=1,
            ylim=c(min(xy[,2])-0.2*diff(range(xy[,2])), max(xy[,2])))
        graphics::legend("bottomleft", bty="n", pch=19, col=Col[c(length(Col),1)],
          legend=c("High", "Low"))

        plot(jitter(y) ~ z, dat, main="Total moose response",
            xlab=i, ylab="Total moose", col="darkgrey", pch=19)
        graphics::lines(zhat ~ z, dat, col=4, lwd=2)

    }

    invisible(NULL)
}

#' Multivariate Exploration
#'
#' @param vars column names from `x` to be used as a predictor
#' @param x data frame with Moose data
#' @param alpha alpha level defining mincriterion = 1 - alpha
#'
#' @export
mc_plot_multivariate <- function(vars, x, alpha=NULL) {
    opts <- getOption("moose_options")
    if (is.null(alpha))
        alpha <- opts$alpha
    z <- data.frame(Y=x[[opts$Ntot]], x[,vars])[x$srv,]
    tr <- partykit::ctree(Y ~ ., z,
        control = partykit::ctree_control(
            mincriterion = 1 - alpha))
    plot(tr)
}

