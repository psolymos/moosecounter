#' Exploration
#'
#' Functions to explore relationships between total Moose or composition
#' as respone vs. environmental predictor variables.
#'
#' `mc_plot_univariate` implements visual univariate
#' (sigle predictor) exploration for the total Moose count models.
#'
#' `mc_plot_multivariate` implements visual multivariate
#' (multiple predictors) exploration based on regression trees
#' (recursive partitioning  in a conditional inference framework)
#' for total Moose counts.
#'
#' `mc_plot_comp` implements visual univariate (sigle predictor) exploration
#' for the multinomial composition models.
#'
#' @param x Data frame with Moose data.
#' @param i Column name from `x` to be used as a predictor.
#' @param vars A vector of column names from `x` to be used as a predictor.
#' @param dist Count distribution (`P`, `NB`, `ZIP`, or `ZINB`).
#' @param alpha Alpha level defining `mincriterion = 1 - alpha` for `partykit::ctree()`.
#'
#' @examples
#' ## Prepare Moose data from Mayo
#' x <- read.csv(
#'     system.file("extdata/MayoMMU_QuerriedData.csv",
#'         package="moosecounter"))
#' switch_response("total")
#' x <- mc_update_total(x)
#'
#' ## Univariate exploration for total Moose
#' mc_plot_univariate("Subalp_Shrub_250buf", x, "ZINB")
#'
#' ## Multivariate exploration for total Moose
#' vars <- c("ELC_Subalpine", "Fire1982_2012", "Fire8212_DEM815",
#'     "NALC_Needle", "NALC_Shrub", "Subalp_Shrub_250buf",
#'     "ELCSub_Fire8212DEM815", "SubShrub250_Fire8212DEM815")
#' mc_plot_multivariate(vars, x)
#'
#' ## Univariate exploration for composition
#' mc_plot_comp("Fire8212_DEM815", x)
#'
#' @name explore
#' @keywords tree models regression
NULL

#' @rdname explore
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

#'
#' @importFrom rlang .data .env
#' @noRd
.plot_univariate <- function(i, x, dist = "ZINB",
                             type = c("density", "map", "fit"),
                             interactive = FALSE) {

  type <- match.arg(type)

  opts <- getOption("moose_options")
  srv <- x$srv
  z <- x[,i]
  CAT <- is.factor(z) || is.character(z)
  if (CAT) {
    z <- droplevels(as.factor(z))
  }
  xy <- x[, opts$xy]
  dat <- data.frame(y = x[srv, opts$Ntot], z = z[srv], SU_ID = x[srv, "SU_ID"])
  dat <- dat[order(dat$z),]
  m <- zeroinfl2(y ~ z | 1, dat, dist=dist, link='logit')
  dat$zhat <- stats::fitted(m)

  if (type == "density") {
    if (CAT) {
      d <- table(z)/length(srv)
      d <- data.frame(Surveyed = "All",
                      Value = names(d), n=as.numeric(d))
      d_srv <- table(z[srv])/sum(srv)
      d_srv <- data.frame(Surveyed = "Surveyed",
                          Value = names(d_srv), n = as.numeric(d_srv))
      dd <- rbind(d, d_srv)

      # Plot
      p <- ggplot2::ggplot(dd,
                           ggplot2::aes(
                             x = .data$Value,
                             y = .data$n,
                             fill = .data$Surveyed,
                             group = .data$Surveyed))

      if(interactive) {
        dd <- dplyr::mutate(dd,
                            tt = paste0("Surveyed: ", .data$Surveyed,
                                        "<br>Density: ", round(.data$n, 2)))

        p <- p + ggiraph::geom_bar_interactive(
          data = dd,
          ggplot2::aes(tooltip = .data$tt, data_id = .data$Surveyed),
          stat = "identity", position = "dodge",
          colour = "black")
      } else {
        p <- p + ggplot2::geom_bar(stat = "identity", position = "dodge",
                                   colour = "black")
      }

    } else {
      d <- data.frame(Surveyed = srv, Variable = z)

      p <- ggplot2::ggplot(
        data = d,
        ggplot2::aes(
          x = .data$Variable,
          fill = .data$Surveyed,
          color = .data$Surveyed)) +
        ggplot2::geom_rug(data = d[!srv, ], sides = "b") +
        ggplot2::geom_rug(data = d[srv, ], sides = "t")

      if(interactive) {
        d <- dplyr::mutate(d, tt = paste0("Surveyed: ", .data$Surveyed))
        p <- p + ggiraph::geom_density_interactive(
          data = d, alpha = 0.50,
          ggplot2::aes(tooltip = .data$tt, data_id = .data$Surveyed))
      } else {
       p <- p + ggplot2::geom_density(alpha = 0.5)
      }
    }

    p <- p +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "top") +
      ggplot2::scale_fill_viridis_d(end = 0.8, aesthetics = c("fill", "colour")) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(i)

  }

  if (type == "fit") {
    if (CAT) {
      p <- ggplot2::ggplot(
        dat,
        ggplot2::aes(
          y = .data$y,
          x = .data$z)) +
        ggplot2::theme_minimal() +
        ggplot2::ylab("Total Moose") +
        ggplot2::xlab(i)

      tot <- stats::aggregate(dat[, c("y", "zhat")], list(z = dat$z), mean)
      tot$z_int <- as.integer(tot$z)

      # TODO: Technically ggiraph plots look the same as ggplot2 plots if not
      #       'served' with girafe(), so we don't really need the `interactive` switch...
      if(interactive) {
        out <- dplyr::group_by(dat, .data$z)
        out <- dplyr::mutate(out,
                             IQR = stats::IQR(.data$y),
                             q75 = stats::quantile(.data$y, 0.75),
                             q25 = stats::quantile(.data$y, 0.25))
        out <- dplyr::filter(out,
                             .data$y > (.data$q75 + (1.5 * .data$IQR)) |
                               .data$y < (.data$q25 - (1.5 * .data$IQR)))
        out <- dplyr::mutate(out, tt = paste0("SU_ID: ", .data$SU_ID,
                                              "<br>", .env$i, ": ", .data$z,
                                              "<br>Total Moose: ", .data$y))
        dat <- dplyr::mutate(dat, tt = .data$z)
        tot <- dplyr::mutate(tot, tt = paste0(.data$z,
                                              "<br>Mean: ", round(.data$zhat, 2)))

        p <- p +
          ggiraph::geom_boxplot_interactive(
            data = dat, fill = "grey", outlier.shape = NA, # omit outliers
            ggplot2::aes(tooltip = .data$tt, data_id = .data$SU_ID)) +
          ggiraph::geom_point_interactive(                 # manually add outliers
            data = out, size = 3, alpha = 0.75,
            ggplot2::aes(tooltip = .data$tt, data_id = .data$SU_ID)) +
          ggiraph::geom_point_interactive(
            data = tot, size = 3, col = 4,
            ggplot2::aes(tooltip = .data$tt, data_id = .data$tt))
      } else {
        p <- p +
          ggplot2::geom_boxplot(fill = "grey")
      }

      # Lines at the end for all plots, never interactive
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(x = .data$z_int, y = .data$zhat),
          data = tot, col = 4)

    } else {
      p <- ggplot2::ggplot(
        dat,
        ggplot2::aes(
          y = .data$y,
          x = .data$z)) +
        ggplot2::geom_line(ggplot2::aes(x = .data$z, y = .data$zhat), col = 4) +
        ggplot2::theme_minimal() +
        ggplot2::ylab("Total Moose") +
        ggplot2::xlab(i)

      dat <- dplyr::mutate(dat, tt = paste0("SU_ID: ", .data$SU_ID,
                                            "<br>", .env$i, ": ", round(.data$z, 2),
                                            "<br>Total Moose: ", .data$y))

      if(interactive){
        p <- p + ggiraph::geom_point_interactive(
          data = dat, size = 3, alpha = 0.75,
          ggplot2::aes(tooltip = .data$tt, data_id = .data$SU_ID))
      } else {
        p <- p + ggplot2::geom_point()
      }

    }
  }
  if (type == "map") {
    p <- ggplot2::ggplot(data = x,
                         ggplot2::aes(x = .data$CENTRLON, y = .data$CENTRLAT,
                                      fill = .data[[i]], alpha = .data$srv)) +
      ggplot2::coord_map() +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "top",
                     legend.title = ggplot2::element_text(size = 9),
                     legend.box.margin = ggplot2::margin(0, 0, -20, 0),
                     plot.margin = ggplot2::unit(c(0,0,0,0), "pt")) +
      ggplot2::scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.65), guide = "none") +
      ggplot2::labs(caption = "Pale points are un-surveyed")

    if(interactive) {
      if(!CAT) x$label <- round(x[[i]], 2) else x$label <- x[[i]]
      x <- dplyr::mutate(x, tt = paste0("SU_ID: ", .data$SU_ID,
                                        "<br>", .env$i, ": ", .data$label))
      p <- p + ggiraph::geom_tile_interactive(
        data = x,
        ggplot2::aes(tooltip = .data$tt, data_id = .data$SU_ID))
    } else {
      p <- p + ggplot2::geom_tile()
    }

    if (CAT) p <- p + ggplot2::scale_fill_viridis_d(end = 0.8)
    if (!CAT) p <- p + ggplot2::scale_fill_binned(type = "viridis")

  }
  p
}

mc_ggiraph <- function(p, width, height, type) {

  # Use `girafe_css` to specify different css for different types
  # - https://www.ardata.fr/ggiraph-book/customize.html#sec-global-opt
  # - https://www.ardata.fr/ggiraph-book/customize.html#detailled-control

  ggiraph::girafe(
    ggobj = p, width_svg = width, height_svg = height,
    options = list(
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_hover(
        css = ggiraph::girafe_css(
          css = "fill:orange;",
          line = "fill:none;stroke:black;",
          point = "fill:orange;fill-opacity:1;r:3pt;stroke-width:3px;stroke-opacity:1;stroke:orange;"))))

}





#' @rdname explore
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

#' @rdname explore
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
