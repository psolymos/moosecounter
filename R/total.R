#' Total Moose Workflow
#'
#' `switch_response` switches between total Moose vs. cows only.
#' This sets the column name for total Moose estimation.
#' `mc_update_total` Updates/prepares the Moose data set for downstream analyses (i.e. calculates some derived variables, sets a surveyed/unsurveyed indicator, and optionally takes a subset).
#' `mc_fit_total` fit total Moose abundance models.
#' `mc_models_total` prints out estimates from the models.
#'
#' @param x A data frame with Moose data,
#'   or a data frame from `mc_update_total()`.
#' @param ml Named list of models.
#' @param coefs logical, return coefficient table too.
#' @param vars column names of `x` to be used as predictors for
#'   the count model.
#' @param zi_vars optional, column names of `x` to be used as
#'   predictors for the zero model.
#' @param dist Count distribution (`P`, `NB`, `ZIP`, `ZINB`).
#' @param weighted Logical, to use weighting to moderate
#'   influential observations.
#' @param intercept Which intercepts to keep. Dropped intercepts lead to
#'   regression through the origin (at the linear predictor scale).
#' @param robust Logical, use robust regression approach.
#' @param ... Other arguments passed to `zeroinfl2()`.
#' @param type The type of the response, can be `"total"` or
#'   `"cows"` for `switch_response`.
#' @param srv Logical vector, rows of `x` that are surveyed,
#'   falls back to global options when `NULL`.
#' @param ss Logical vector to subset `x`, default is to take no subset.
#' @param model_id model ID or model IDs (can be multiple from `names(ml)`).
#' @param do_boot Logical, to do bootstrap or not.
#' @param do_avg Logical, to do model averaging or not.
#' @param PI PI object returned by `mc_predict_total()`
#' @param i Column (variable) name or index.
#' @param interactive Logical, draw interactive plot.
#' @param id Cell ID.
#' @param plot Logical, to plot or just give summary.
#' @param breaks Breaks argument passed to `graphics::hist()`.
#'
#' @examples
#'
#' mc_options(B=10)
#'
#' x <- read.csv(
#'     system.file("extdata/MayoMMU_QuerriedData.csv",
#'         package="moosecounter"))
#'
#' #switch_response("cows") # for cows only
#' switch_response("total")
#'
#' x <- mc_update_total(x)
#'
#' mc_plot_univariate("Subalp_Shrub_250buf", x, "ZINB")
#'
#' vars <- c("ELC_Subalpine", "Fire1982_2012", "Fire8212_DEM815",
#'     "NALC_Needle", "NALC_Shrub", "Subalp_Shrub_250buf",
#'     "ELCSub_Fire8212DEM815", "SubShrub250_Fire8212DEM815")
#'
#' mc_plot_multivariate(vars, x)
#'
#' ML <- list()
#' ML[["Model 0"]] <- mc_fit_total(x, dist="ZINB", weighted=TRUE)
#' ML[["Model 1"]] <- mc_fit_total(x, vars[1:2], dist="ZINB", weighted=TRUE)
#' ML[["Model 2"]] <- mc_fit_total(x, vars[2:3], dist="ZIP", weighted=TRUE)
#' ML[["Model 3"]] <- mc_fit_total(x, vars[3:4], dist="ZINB", weighted=TRUE)
#'
#' mc_models_total(ML, x)
#' mc_plot_residuals("Model 3", ML, x)
#'
#' PI <- mc_predict_total(
#'     model_id=c("Model 1", "Model 3"),
#'     ml=ML,
#'     x=x,
#'     do_boot=TRUE, do_avg=TRUE)
#'
#' mc_get_pred(PI)
#' pred_density_moose_PI(PI)
#' mc_plot_predpi(PI)
#' mc_plot_pidistr(PI)
#' mc_plot_pidistr(PI, id=2)
#'
#' @keywords models regression
#' @name total
NULL

#' @rdname total
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

#' @rdname total
#' @export
# used to be saveMooseData
mc_update_total <- function(x, srv=NULL, ss=NULL) {
  opts <- getOption("moose_options")
  if (is.null(srv)) {
    srv <- x[[opts$srv_name]] == opts$srv_value
  }
  x$srv <- srv
  ## this is needed for COW analysis
  x$COW_TOTA <- rowSums(x[,c("LONE_COW", "COW_1C", "COW_2C")])
  if (!is.null(ss))
    x <- x[ss,]
  x
}

#' @rdname total
#' @export
mc_fit_total <- function(x, vars=NULL, zi_vars=NULL,
    dist="ZINB", weighted=FALSE, robust=FALSE,
    intercept = c("both", "count", "zero", "none"), ...) {
    intercept <- match.arg(intercept)
    opts <- getOption("moose_options")
    if (is.null(vars)) {
        if (intercept %in% c("zero", "none"))
            warning("Cannot remove intercept for the count model.")
        CNT <- "1"
    } else {
        vars <- vars[!(vars %in% c(opts$Ntot, opts$composition))]
        CNT <- paste(vars, collapse=" + ")
        if (intercept %in% c("zero", "none"))
            CNT <- paste0(CNT, " - 1")
    }
    if (is.null(zi_vars)) {
        if (intercept %in% c("count", "none"))
            warning("Cannot remove intercept for the zero model.")
        ZI <- "1"
    } else {
        zi_vars <- zi_vars[!(zi_vars %in% c(opts$Ntot, opts$composition))]
        ZI <- paste(zi_vars, collapse=" + ")
        if (intercept %in% c("count", "none"))
            ZI <- paste0(ZI, " - 1")
    }
    Form <- stats::as.formula(paste(opts$Ntot, "~", CNT, "|", ZI))
    out <- zeroinfl2(
        formula=Form,
        data=x[x$srv,],
        dist=dist,
        link="logit",
        method=opts$method,
        robust=robust,
        ...)
    if (weighted)
        out <- wzi(out) # wzi know about method
    out$call <- match.call()
    out
}

## organize coefs from ZI models
get_coefs <- function(ML) {
    l <- lapply(ML, stats::coef)
    cfn <- unique(unname(unlist(lapply(l, names))))
    cfn1 <- cfn[startsWith(cfn, "count_")]
    cfn1 <- c("count_(Intercept)", cfn1[cfn1 != "count_(Intercept)"])
    cfn0 <- cfn[startsWith(cfn, "zero_")]
    cfn0 <- c("zero_(Intercept)", cfn0[cfn0 != "zero_(Intercept)"])
    cfn <- c(cfn1, cfn0)
    M <- matrix(NA_real_, length(ML), length(cfn))
    dimnames(M) <- list(names(ML), cfn)
    for (i in seq_len(length(ML))) {
        M[i,] <- l[[i]][match(cfn, names(l[[i]]))]
    }
    colnames(M) <- gsub("(", "", gsub(")", "", colnames(M), fixed=TRUE), fixed=TRUE)
    M
}

#' @rdname total
#' @export
# was updateModelTab
mc_models_total <- function(ml, x, coefs=TRUE) {
    aic <- sapply(ml, stats::AIC)
    bic <- sapply(ml, stats::BIC)
    ic <- data.frame(
        ic=aic,
        BIC=bic,
        df=sapply(ml, function(z) length(stats::coef(z))),
        logLik=sapply(ml, function(z) as.numeric(stats::logLik(z))))
    ic$delta <- ic$ic - min(ic$ic)
    ic$weight <- exp( -0.5 * ic$delta) / sum(exp( -0.5 * ic$delta))
    colnames(ic)[colnames(ic) == "ic"] <- "AIC"

    D <- t(sapply(ml, pred_density_moose, x=x))
    out <- data.frame(ic, D)
    if (coefs) {
      cf <- get_coefs(ml)
      out <- data.frame(out, cf)
    }
    out[order(out$delta),]
}


## predict total moose abundance and density
pred_total_moose <- function(x, surveyed, fit){
    opts <- getOption("moose_options")
    ## predict response for unsurveyed cells
    pr_uns <- stats::predict(fit, newdata=x[!surveyed,,drop=FALSE], type=c("response"))
    ## sum observed + predicted
    pr_total <- sum(pr_uns) + sum(x[[opts$Ntot]][surveyed])
    ## here is the sightability
    s <- opts$sightability
    out <- list(
#        pr_srv = fitted(fit),
        pr_uns = pr_uns / s,
        Ntot_srv = sum(x[[opts$Ntot]][surveyed]) / s,
        Ntot_uns = sum(pr_uns) / s) # this is kind of redundant
    out$Ntot_all <- out$Ntot_srv + out$Ntot_uns
    out
}

# x: MooseData
pred_density_moose <- function(fit, x){
    opts <- getOption("moose_options")
    srv <- x$srv
    Ntot <- pred_total_moose(x, srv, fit)
    A_all <- sum(x$AREA_KM)
    Density <- Ntot$Ntot_all / (1*A_all)
    out <- c(N=Ntot$Ntot_all, A=A_all, D=Density)
    names(out) <- if (opts$response == "total") {
        c("Total_Moose", "Total_Area_km2", "Density_Moose_Per_km2")
    } else {
        c("Total_Cows", "Total_Area_km2", "Density_Cows_Per_km2")
    }
    out
}


#' @rdname total
#' @export
# was: MooseSim.PI
# x: MooseData
mc_predict_total <- function(model_id, ml, x, do_boot=TRUE, do_avg=FALSE) {
    wt <- mc_models_total(ml, x)
    if (!any(model_id %in% rownames(wt)))
        stop("model_id not recognized")
    wts <- wt[model_id,,drop=FALSE]
    opts <- getOption("moose_options")
    model_id0 <- model_id
    ## if some survey area is defined, use dual prediction
    DUAL <- !is.null(opts$area_srv)
    if (DUAL) {
        x$area_srv <- x[[opts$area_srv]]
    } else {
        x$area_srv <- TRUE
    }
    x$sort_id <- 1:nrow(x)
    srv <- x$srv
    x$observed_values <- NA
    x$observed_values[srv] <- ml[[1]]$y # all models should have same data
    x$fitted_values <- NA
#    x$fitted_values[srv] <- fit$fitted.values
    B <- opts$B
    MAXCELL <- if (is.null(opts$MAXCELL))
        2*max(x[srv,opts$Ntot], na.rm=TRUE) else opts$MAXCELL
    if (MAXCELL < max(x[srv,opts$Ntot], na.rm=TRUE))
        stop("MAXCELL must not be smaller than max observed total abundance")
    alpha <- opts$alpha

    NS <- sum(!srv)

    ## Simulated prediction intervals for the Unsurveyed cells
    ## accounting for the estimation uncertainty.

    x_srv <- x[srv,]
    x_uns <- x[!srv,]
    boot.out <- matrix(0, NS, B)
    fit.out <- matrix(0, sum(srv), B)
    mid <- character(B)
    b <- 1

    # Progress bars setup
    if(requireNamespace("shiny") && shiny::isRunning()) {
      pbapply::pboptions(type = "shiny",
                         title = "Calculating prediction intervals")
    }
    pb <- pbapply::startpb(0, B)
    on.exit(pbapply::closepb(pb))

    ISSUES <- list()

    while (b <= B) {

        ## model selection
        if (do_avg) {
            model_id <- sample(rownames(wts), 1, prob=wts$weight)
        } else {
            model_id <- rownames(wts)[which.max(wts$weight)]
            if (length(model_id) > 1) # this should really never happen
                model_id <- sample(model_id, 1)
        }
        mid[b] <- model_id
        #cat("Fitting #", b, " of ", model_id, "\n", sep="")

        if (!(model_id %in% names(ml)))
            stop(model_id, " model cannot be found")
        fit <- ml[[model_id]]

        fit.out[,b] <- fit$fitted.values

        parms.start <- list(count = fit$coef$count,
            zero = fit$coef$zero,
            theta = fit$theta)

        if (do_boot) {
            BSurvey.data <- x_srv[sample.int(nrow(x_srv), nrow(x_srv),
                replace=TRUE),]
        } else {
            BSurvey.data <- x_srv
        }
        if (max(BSurvey.data[[opts$Ntot]]) != 0){

            if (do_boot) {
                model.Boot <- try(suppressWarnings(stats::update(fit,
                    x = BSurvey.data,
                    weights=rep(1, nrow(BSurvey.data)),
                    control = pscl::zeroinfl.control(
                        start = parms.start,
                        method = opts$method))), silent = TRUE)
                if (!inherits(model.Boot, "try-error")) {
                    attr(model.Boot, "parms.start") <- parms.start
                    if (inherits(fit, "wzi"))
                        model.Boot <- wzi(model.Boot, pass_data=TRUE)
                }
            } else {
                model.Boot <- fit
            }

            if (!inherits(model.Boot, "try-error")) {
                predict.BNS <- stats::predict(model.Boot, newdata=x_uns, type="response")
                predict.BNSout <- if (DUAL && inherits(fit, "wzi")) {
                    stats::predict(model.Boot$unweighted_model,
                        newdata=x_uns, type="response")
                } else {
                    predict.BNS
                }

                if (max(predict.BNS, predict.BNSout) <= MAXCELL && model.Boot$optim$convergence == 0) {
                    Bm.NS <- stats::predict(model.Boot, newdata = x_uns, type="count")
                    Btheta.nb <- model.Boot$theta
                    Bphi.zi <- 1 - stats::predict(model.Boot, newdata = x_uns, type="zero")
                    boot.out[,b] <- rZINB(NS,
                        mu.nb = Bm.NS,
                        theta.nb=Btheta.nb,
                        phi.zi=Bphi.zi) # this os prob of 1 (not 0) and is correct
                    if (DUAL && inherits(fit, "wzi")) {
                        Bm.NSout <- stats::predict(model.Boot$unweighted_model,
                                            newdata = x_uns[!x_uns$area_srv,], type="count")
                        Btheta.nbout <- model.Boot$unweighted_model$theta
                        Bphi.ziout <- 1 - stats::predict(model.Boot$unweighted_model,
                                               newdata = x_uns[!x_uns$area_srv,], type="zero")
                        boot.out[!x_uns$area_srv,b] <- rZINB(sum(!x_uns$area_srv),
                            mu.nb = Bm.NSout,
                            theta.nb=Btheta.nbout,
                            phi.zi=Bphi.ziout)
                    }
                    if (max(boot.out[,b]) <= MAXCELL) {
                        pbapply::setpb(pb, b)
                        b <- b + 1
                    }
                }
            } else {
                ISSUES[[length(ISSUES)+1]] <- as.character(model.Boot)
            }
        }
    }

    s <- opts$sightability
    TOTA <- x_srv[[opts$Ntot]] / s
    boot.out <- boot.out / s

    TotalMoose.dist <- apply(boot.out, 2, sum)
    Cell.PI <- apply(boot.out, 1, stats::quantile, c(alpha/2, 0.5, (1-alpha/2)))
    x_uns$Cell.mean <- rowMeans(boot.out)
    x_uns$Cell.mode <- apply(boot.out, 1, find_mode)
    x_uns$Cell.pred <- Cell.PI[2,]
    x_uns$Cell.PIL <- Cell.PI[1,]
    x_uns$Cell.PIU <- Cell.PI[3,]
    x_uns$Cell.accuracy <- Cell.PI[3,] - Cell.PI[1,]
    #x_uns$Cell.perc.accuracy <- 0.5*x_uns$Cell.accuracy / (x_uns$Cell.pred + 1)
    x_uns$Rank <- as.integer(as.factor(rank(-x_uns$Cell.accuracy)))

    x_srv$fitted_values <- rowMeans(fit.out)
    x_srv$Cell.mean <- TOTA
    x_srv$Cell.mode <- TOTA
    x_srv$Cell.pred <- TOTA
    x_srv$Cell.PIL <- TOTA
    x_srv$Cell.PIU <- TOTA
    x_srv$Cell.accuracy <- 0
    x_srv$Rank <- NA

    x_full <- rbind(x_srv, x_uns)
    boot.srv <- matrix(TOTA, nrow(x_srv), ncol(boot.out))
    boot.full <- rbind(boot.srv, boot.out)
    x_full$keep <- TRUE

    o <- order(x_full$sort_id)
    out <- list(model_id=model_id0,
        do_avg=do_avg,
        do_boot=do_boot,
        model_list=ml,
        model_select_id=mid,
        alpha=alpha,
        boot_full=boot.full[o,,drop=FALSE],
        issues=ISSUES,
#        fit_matrix=if (do_avg) fit.out else NULL, # see fitted_values when NULL
        data=x_full[o,,drop=FALSE])

    csfull <- colSums(boot.full)
    tmPI <- c(Mean=mean(csfull),
        Median=unname(stats::quantile(csfull, 0.5)),
        Mode=find_mode(csfull),
        stats::quantile(csfull, c(alpha/2, (1-alpha/2))))
    out$total <- rbind(N=tmPI,
        A=sum(x_full[[opts$Area]]),
        D=tmPI / (1 * sum(x_full[[opts$Area]])))
    rownames(out$total) <- if (opts$response == "total") {
        c("Total_Moose", "Total_Area_km2", "Density_Moose_Per_km2")
    } else {
        c("Total_Cows", "Total_Area_km2", "Density_Cows_Per_km2")
    }
    out
}

#savePiData <- function(PI) {
#  tmp <- PI$data[!PI$data$srv,,drop=FALSE]
#  tmp <- tmp[order(tmp$Cell.accuracy, decreasing=TRUE),]
#  assign("PiData", tmp, envir=.GlobalEnv)
#  invisible(NULL)
#}
## this is used for subset, to avoid confusion
#savePiDataSubset <- function(PI) {
#  tmp <- PI$data[!PI$data$srv,,drop=FALSE]
#  tmp <- tmp[order(tmp$Cell.accuracy, decreasing=TRUE),]
#  assign("PiDataSubset", tmp, envir=.GlobalEnv)
#  invisible(NULL)
#}

#' @rdname total
#' @export
# was: subsetPiData
mc_get_pred <- function(PI, ss=NULL) {
    if (is.null(ss))
        ss <- rep(TRUE, nrow(PI$data))
    opts <- getOption("moose_options")
    PIout <- PI
    PIout$boot_full <- PI$boot_full[ss,,drop=FALSE]
    PIout$data <- PI$data[ss,,drop=FALSE]
    PIout$data$Residuals <- PIout$data$fitted_values - PIout$data$observed_values
#    PIout$fit_matrix <- NULL
    Rank <- PIout$data$Rank[]
    PIout$data$Rank[!PIout$data$srv] <- as.integer(as.factor(
        rank(-PIout$data$Cell.accuracy[!PIout$data$srv])))

    csfull <- colSums(PIout$boot_full)
    alpha <- getOption("moose_options")$alpha
    tmPI <- c(Mean=mean(csfull),
        Median=unname(stats::quantile(csfull, 0.5)),
        Mode=find_mode(csfull),
        stats::quantile(csfull, c(alpha/2, (1-alpha/2))))
    PIout$total <- rbind(N=tmPI,
        A=sum(PIout$data[[opts$Area]]),
        D=tmPI/sum(PIout$data[[opts$Area]]))
    PIout
}

#' @rdname total
#' @export
pred_density_moose_PI <- function(PI) {
    out <- round(PI$total, 2)
    cat("Total Moose PI summary:\n\n")
    print(out)
    if (length(PI$issues) > 0L) {
        cat("\nNote:", length(PI$issues),
            "issues were found during PI calculations.\n\n")
    }
    invisible(out)
}

#' @rdname total
#' @export
# was: plotResiduals
mc_plot_residuals <- function(model_id, ml, x) {
    fit <- ml[[model_id]]
    opts <- getOption("moose_options")
    srv <- x$srv
    Y <- fit$y
    Mean <- fit$fitted.values
    z <- (Y - Mean) / sqrt(Y + 0.5)
    xy <- x[!srv, opts$xy]

    Colfun <- grDevices::colorRampPalette(
        c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'))
    AbsMax <- max(abs(z))
    Sd <- stats::sd(z)
    nn <- ceiling(AbsMax / Sd)
    br <- seq(-nn*Sd, nn*Sd, by=0.5*Sd)
    tmp <- graphics::hist(z, plot=FALSE, breaks=br)
    Col <- Colfun(length(tmp$counts))
    tz <- tanh(z)
    ctz <- cut(z, br)

    op <- graphics::par(mfrow=c(1,2))
    on.exit(graphics::par(op))

    plot(x[srv, opts$xy], pch=19, col=Col[ctz], cex=0.5+1.5*abs(tz),
      xlab="Longitude", ylab="Latitude", #asp=1,
      main=paste("Residuals for Model ID:", model_id),
      ylim=range(x[,opts$xy[2]])-c(0.2*diff(range(x[,opts$xy[2]])), 0))
    graphics::points(x[!srv,opts$xy], pch="+", col="grey")
    graphics::legend("bottomleft", pch=c("o","+"), col="grey",
      bty="n", legend=c("Surveyed", "Unsurveyed"))

    lo <- x[srv,,drop=FALSE]
    lo <- lo[z <= (-1.5*Sd),,drop=FALSE]
    if (nrow(lo) > 0)
        graphics::text(lo[, opts$xy], labels = lo$SU_ID, cex=0.8)
    hi <- x[srv,,drop=FALSE]
    hi <- hi[z >= 1.5*Sd,,drop=FALSE]
    if (nrow(hi) > 0)
        graphics::text(hi[, opts$xy], labels = hi$SU_ID, cex=0.8)

    graphics::hist(z, xlab="Standardized Residuals", col=Col, breaks=br,
      main=paste("Model ID:", model_id))

    invisible(z)
}

#' @rdname total
#' @export
mc_plot_predpi <- function(PI) {

    opts <- getOption("moose_options")
    x <- PI$data
    if (nrow(x) < 1)
        stop("subset is empty")
    srv <- x$srv
    B <- opts$B
    #MAXCELL <- opts$MAXCELL
    alpha <- opts$alpha

    Y <- x$observed_values[srv]
    Mean <- x$fitted_values[srv]
    z <- (Y - Mean) / sqrt(Y + 0.5)
    xy <- x[!srv, opts$xy]

    Colfun <- grDevices::colorRampPalette(
        c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'))
    AbsMax <- max(abs(z))
    Sd <- stats::sd(z)
    nn <- ceiling(AbsMax / Sd)
    br <- seq(-nn*Sd, nn*Sd, by=0.5*Sd)
    tmp <- graphics::hist(z, plot=FALSE, breaks=br)
    Col <- Colfun(length(tmp$counts))
    tz <- tanh(z)
    ctz <- cut(z, br)

    op <- graphics::par(mfrow=c(1,3))
    on.exit(graphics::par(op))

    ModID <- if (length(unique(PI$model_select_id))>1)
        "Avg" else unique(PI$model_select_id)
    plot(x[srv, opts$xy], pch=19, col=Col[ctz], cex=0.5+1.5*abs(tz),
        xlab="Longitude", ylab="Latitude", #asp=1,
        main=paste("Residuals for Model ID:", ModID),
        ylim=range(x[,opts$xy[2]])-c(0.2*diff(range(x[,opts$xy[2]])), 0))
    graphics::points(x[!srv,opts$xy], pch="+", col="grey")
    graphics::legend("bottomleft", pch=c("o","+"), col="grey",
        bty="n", legend=c("Surveyed", "Unsurveyed"))

    lo <- x[srv,,drop=FALSE]
    lo <- lo[z <= (-1.5*Sd),,drop=FALSE]
    if (nrow(lo) > 0)
        graphics::text(lo[, opts$xy], labels = lo$SU_ID, cex=0.8)
    hi <- x[srv,,drop=FALSE]
    hi <- hi[z >= 1.5*Sd,,drop=FALSE]
    if (nrow(hi) > 0)
        graphics::text(hi[, opts$xy], labels = hi$SU_ID, cex=0.8)

    graphics::hist(z, breaks=br, xlab="Standardized Residuals", col=Col,
        main=paste("Model ID:", ModID))

    xy <- PI$data[!srv, opts$xy]
    Nobs <- x[srv, opts$Ntot]
    Npred <- PI$data[!srv, "Cell.pred"]
    Acc <- PI$data[!srv, "Cell.accuracy"]
    AccRank <- PI$data[!srv, "Rank"]
    Max <- max(c(Nobs, Npred))
    zNobs <- Nobs / Max
    zNpred <- Npred / Max
    zAcc <- (Acc - min(Acc)) / diff(range(Acc))

    Colfun <- grDevices::colorRampPalette(
        c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026'))
    Col <- Colfun(9)
    #Col <- brewer.pal(9, "YlOrRd")[1:5]
    czAcc <- cut(zAcc, c(-0.1, 0.2, 0.4, 0.6, 0.8, 1.1))

    Min <- sapply(1:20, function(z) length(which(AccRank <= z)))
    Min <- min(20, (1:20)[min(which(Min >= 20))])

    plot(xy, pch=19, col=Col[czAcc], cex=1+zAcc*1,
      ylim=range(x[,opts$xy[2]])-c(0.2*diff(range(x[,opts$xy[2]])), 0),
      xlab="Longitude", ylab="Latitude", #asp=1,
      main=paste("Accuracy for Model ID:", ModID))
    graphics::points(x[srv,opts$xy], pch="+", col="grey")
#    text(x[!srv,opts$xy][AccRank <= Min,], labels=AccRank[AccRank <= Min], cex=1)
    graphics::text(x[!srv,opts$xy][AccRank <= Min,], labels=x[!srv,"SU_ID"][AccRank <= Min], cex=1)
    graphics::legend("bottomleft", pch=19, col=Col[c(5,3,1)],
      bty="n", legend=c("+++", "++", "+"))

    invisible(PI)
}

#' @rdname total
#' @export
mc_plot_pidistr <- function(PI, id=NULL, plot=TRUE, breaks="Sturges") {
    if (is.null(id)) {
        .mc_plot_pidistrall(PI=PI, plot=plot, breaks=breaks)
    } else {
        .mc_plot_pidistrcell(PI=PI, id=id, plot=plot, breaks=breaks)
    }
}

.mc_plot_pidistrall <- function(PI, plot=TRUE, breaks="Sturges") {
    csfull <- colSums(PI$boot_full)
    if (plot) {
        h <- graphics::hist(csfull, breaks=breaks, plot=FALSE)
        h$density <- h$counts * 100 / sum(h$counts)
        if (length(unique(csfull)) == 1) {
            h$mids <- unique(csfull)
            h$breaks <- unique(csfull) + c(-0.5, 0.5)
        }
        d <- stats::density(csfull)
        d$y <- max(h$density) * d$y / max(d$y)
        plot(h,
            freq=FALSE, col="lightgrey", main="Total Moose PI",
            xlab="Predicted Total Moose", ylab="Percent",
            border="darkgrey",
            ylim=c(0, max(h$density, d$y)))

        graphics::lines(d)
        graphics::rug(csfull, col=1)
        graphics::abline(v=PI$total["Total_Moose", "Mean"], col=2)
        graphics::abline(v=PI$total["Total_Moose", "Median"], col=3)
        graphics::abline(v=PI$total["Total_Moose", "Mode"], col=4)
        graphics::abline(v=PI$total["Total_Moose", 4:5], col="darkgrey", lty=2)
        TXT <- paste0(c("Mean", "Median", "Mode"), " = ",
            format(PI$total["Total_Moose", c("Mean", "Median", "Mode")],
                   nsmall = 3))
        graphics::legend("topright", lty=c(1,1,1,2), col=c(2:4, "darkgrey"), bty="n",
            legend=c(TXT, paste0(100-100*PI$alpha, "% PI")))
    }
    invisible(csfull)
}

.mc_plot_pidistrcell <- function(PI, id=1, plot=TRUE, breaks="Sturges") {
    csfull <- PI$boot_full[id,]
    is_srv <- PI$data$srv[id]
    if (plot) {
        if (is_srv) {
            graphics::plot.new()
            graphics::title(main=paste("Moose PI for Cell", id))
            graphics::text(0.5, 0.5, paste("Observed Count =", csfull[1]))
        } else {
            h <- graphics::hist(csfull, breaks=breaks, plot=FALSE)
            h$density <- h$counts * 100 / sum(h$counts)
            if (length(unique(csfull)) == 1) {
                h$mids <- unique(csfull)
                h$breaks <- unique(csfull) + c(-0.5, 0.5)
            }
            d <- stats::density(csfull)
            d$y <- max(h$density) * d$y / max(d$y)
            plot(h,
                freq=FALSE, col="lightgrey",
                main=paste("Moose PI for Cell", id),
                xlab="Predicted Total Moose in cell", ylab="Percent",
                border="darkgrey",
                ylim=c(0, max(h$density, d$y)))
            graphics::lines(d)
            graphics::rug(csfull, col=1)
            graphics::abline(v=PI$data[id, "Cell.mean"], col=2)
            graphics::abline(v=PI$data[id, "Cell.pred"], col=3)
            graphics::abline(v=PI$data[id, "Cell.mode"], col=4)
            graphics::abline(v=PI$data[id, c("Cell.PIL", "Cell.PIU")], col="darkgrey", lty=2)
            TXT <- paste0(c("Mean", "Median", "Mode"), " = ",
                format(PI$data[id, c("Cell.mean", "Cell.pred", "Cell.mode")],
                       nsmall = 3))
            graphics::legend("topright", lty=c(1,1,1,2), col=c(2:4, "darkgrey"), bty="n",
                legend=c(TXT, paste0(100-100*PI$alpha, "% PI")))
        }
    }
    invisible(csfull)
}


#' @rdname total
#' @export
mc_plot_predfit <- function(i, PI, ss = NULL, interactive = FALSE) {

  dat <- PI$data[, c("SU_ID", "Cell.mean", "Cell.PIL", "Cell.PIU", "srv", i)]
  names(dat)[names(dat) == "srv"] <- "Surveyed"
  names(dat)[names(dat) == i] <- "z"
  if(!is.null(ss)) dat <- dat[ss, , drop = FALSE]

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$z, y = .data$Cell.mean,
                                         colour = .data$Surveyed)) +
    ggplot2::geom_ribbon(data = dplyr::filter(dat, !.data$Surveyed),
                         mapping = ggplot2::aes(ymin = .data$Cell.PIL, ymax = .data$Cell.PIU,
                                                fill = "Prediction Interval"),
                         alpha = 0.15, colour = NA) +
    ggplot2::scale_fill_manual(name = NULL, values = "black") +
    ggplot2::scale_colour_viridis_d(end = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::xlab(i) +
    ggplot2::ylab("Total Moose")

  if(interactive) {
    dat <- dplyr::mutate(dat,
                         tt = paste0("SU_ID: ", .data$SU_ID,
                                     "<br>", .env$i, ": ", round(.data$z, 2),
                                     "<br>Total Moose: ", .data$Cell.mean,
                                     "<br>Surveyed: ", .data$Surveyed))
    p <- p + ggiraph::geom_point_interactive(data = dat, size = 3, alpha = 0.5,
                                             ggplot2::aes(tooltip = .data$tt,
                                                          data_id = .data$tt))
    p <- ggiraph::girafe(ggobj = p, width_svg = 8, height_svg = 4)
  } else {
    p <- p + ggplot2::geom_point()
  }
  p
}

