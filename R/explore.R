#' Exploration
#'
#' `mc_plot_univariate` implements visual univariate
#' (sigle predictor) exploration for the total Moose count models.
#'
#' @param i column name from `x` to be used as a predictor
#' @param x data frame with Moose data
#' @param dist count distribution (P, NB, ZIP, or ZINB)
#'
#' @rdname explore
#' @keywords tree models regression
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
            xlab="Longitude", ylab="Latitude", #asp=1,
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
            xlab="Longitude", ylab="Latitude", #asp=1,
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
#' `mc_plot_multivariate` implements visual multivariate
#' (multiple predictors) exploration based on regression trees
#' (recursive partitioning  in a conditional inference framework)
#' for total Moose counts.
#'
#' @param vars column names from `x` to be used as a predictor
#' @param x data frame with Moose data
#' @param alpha alpha level defining mincriterion = 1 - alpha for `partykit::ctree()`
#'
#' @rdname explore
#' @keywords tree models regression
#' @export
mc_plot_multivariate <- function(vars, x, alpha=NULL) {
    opts <- getOption("moose_options")
    if (is.null(alpha))
        alpha <- opts$alpha
    z <- data.frame(Y=x[[opts$Ntot]], x[,vars,drop=FALSE])[x$srv,]
    tr <- partykit::ctree(Y ~ ., z,
        control = partykit::ctree_control(
            mincriterion = 1 - alpha))
    plot(tr)
}

#' Plot Univariate Composition Model
#'
#' `mc_plot_comp` implements visual univariate (sigle predictor) exploration
#' for the multinomial composition models.
#'
#' @param i predictor variable name in `x`
#' @param x moose data frame
#'
#' @rdname explore
#' @keywords tree models regression
#' @export
mc_plot_comp <- function(i, x) {
    NAME <- i
    MooseData <- x
    #formula <- stats::as.formula(paste("~", NAME))
    #vlm <- VGAM::vlm
    m <- mc_fit_comp(MooseData, NAME)
    mf <- VGAM::model.frame(m)
    pr0 <- VGAM::fitted(m)
    xx <- mf[,NAME]

    col <- c("#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0","#f0027f")
    if (is.numeric(MooseData[,NAME])) {
        i <- order(xx)
        xx <- xx[i]
        pr0 <- pr0[i,]
        pr <- t(apply(pr0, 1, cumsum))
        op <- graphics::par(mfrow=c(1,3), las=1)
        pr2 <- cbind(0, pr)
        graphics::matplot(xx, 100*pr2, type="l", lty=1, main="Composition", xlab=NAME,
            ylab="Percent of Total", col=col, axes=FALSE)
        graphics::axis(1)
        graphics::axis(2)
        for (i in 1:6) {
            graphics::polygon(c(xx, rev(xx)), 100*c(pr2[,i], rev(pr2[,i+1])), col=col[i])
        }
        graphics::matplot(xx, 100*pr[,1:5], type="l", lty=1, col=1, add=TRUE)

        graphics::matplot(xx, 100*pr0, type="l", lty=1, lwd=3, main="", xlab=NAME,
            ylab="Percent of Total", col=col, ylim=c(0,100*max(pr0)))

        graphics::plot.new()
        graphics::legend("topleft", fill=rev(col), legend=rev(colnames(mf$ymat)))
        graphics::par(op)
    } else {
        i <- !duplicated(xx)
        xx <- xx[i]
        pr0 <- t(pr0[i,])
        colnames(pr0) <- as.character(xx)
        op <- graphics::par(mfrow=c(1,3), las=1)
        graphics::barplot(100*pr0, space=0, names.arg=xx, main="Composition", xlab=NAME,
            ylab="Percent of Total",col=col)

        graphics::matplot(1:ncol(pr0), 100*t(pr0), type="b", lty=2, lwd=3, main="",
            xlab=NAME, axes=FALSE, pch=19, xlim=c(0.5, ncol(pr0)+0.5),
            ylab="Percent of Total", col=col, ylim=c(0,100*max(pr0)))
        graphics::axis(1, at=1:ncol(pr0), labels=colnames(pr0))
        graphics::axis(2)
        graphics::box()

        graphics::plot.new()
        graphics::legend("topleft", fill=rev(col), legend=rev(colnames(mf$ymat)))
        graphics::par(op)
    }
    invisible(NULL)
}
