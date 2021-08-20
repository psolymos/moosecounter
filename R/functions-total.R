## switch total/cows

solvenear <- function (x) {
    if (is.null(x))
        return(NULL)
    xinv <- try(solve(x), silent = TRUE)
    if (inherits(xinv, "try-error"))
        xinv <- as.matrix(solve(Matrix::nearPD(x)$mat))
    xinv
}

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
    moose_options(opts)
}

## density plots

plotUnivariateExpl <- function(colid, dist='negbin') {

    x <- MooseData
    srv <- MooseData$srv
    opts <- getOption("moose_options")
    d <- density(x[,colid])
    d_srv <- density(x[srv,colid])
    d_uns <- density(x[!srv,colid])

    ylim <- c(0, max(max(d_srv$y), max(d_uns$y)))
    xlim <- c(0, max(max(d_srv$x), max(d_uns$x)))

    z <- x[,colid]
    xy <- x[, opts$xy]
    Col <- rev(heat.colors(10))
    dat <- data.frame(y=x[srv,opts$Ntot], z=x[srv,colid])
    dat <- dat[order(dat$z),]
    m <- zeroinfl2(y ~ z | 1, dat, dist=dist, link='logit')
    dat$zhat <- fitted(m)

    op <- par(mfrow=c(1,3))
    plot(d, xlim=xlim,
      ylim=ylim + c(0, 0.2*diff(ylim)), xlab=colid, main="Density")
    lines(d_srv, col=2)
    rug(x[,colid], col=1, side=1)
    rug(x[srv,colid], col=2, side=3)
    #lines(d_uns, col=4)
    legend("topleft", bty="n", lty=1, col=c(1,2),
        legend=c("All","Surveyed"))

    plot(xy, pch=19, col=Col[cut(z, 10)], main="Map",
        xlab="Longitude", ylab="Latitude",
        ylim=c(min(xy[,2])-0.2*diff(range(xy[,2])), max(xy[,2])))
    legend("bottomleft", bty="n", pch=19, col=Col[c(length(Col),1)],
      legend=c("High", "Low"))

    plot(jitter(y) ~ z, dat, main="Total moose response",
        xlab=colid, ylab="Total moose", col="darkgrey", pch=19)
    lines(zhat ~ z, dat, col=4, lwd=2)

    par(op)
    invisible(NULL)
}

wzi <- function(object, pass_data=FALSE, ...) {
    wscale <- getOption("moose_options")$wscale
    n <- nobs(object)
    ll0 <- as.numeric(logLik(object))
    ll <- numeric(n)
    w <- rep(1, n)
    ctrl <- zeroinfl.control(
        method = getOption("moose_options")$method,
        start = attr(object, "parms.start"))
    d <- model.frame(object)
    for (i in seq_len(n)) {
        m <- try(suppressWarnings(update(object, data=d[-i,,drop=FALSE],
            weights=w[-i], control=ctrl)), silent=TRUE)
        ll[i] <- if (inherits(m, "try-error"))
            (n-1)*ll0/n else as.numeric(logLik(m))
    }
    w <- 1/abs(ll0-ll)^wscale
    w <- n*w/sum(w)
    if (pass_data) {
        out <- try(suppressWarnings(update(object, data=d,
            weights=w, control=ctrl)), silent=TRUE)
        if (inherits(out, "try-error"))
            out <- object
    } else {
        out <- try(suppressWarnings(update(object,
            weights=w, control=ctrl)), silent=TRUE)
        if (inherits(out, "try-error"))
            out <- object
    }
    out$unweighted_model <- object
    class(out) <- c("wzi", class(out))
    out
}

## define ZINB model

checkModelList <- function() {
	if (!exists("ModelList"))
			assign("ModelList", list(), envir=.GlobalEnv)
	invisible(NULL)
}
saveMooseData <- function(x, srv, ss=NULL, force=FALSE) {
  x$srv <- srv
  ## this is needed for COW analysis
  x$COW_TOTA <- rowSums(x[,c("LONE_COW", "COW_1C", "COW_2C")])
  if (!is.null(ss))
    x <- x[ss,]
  if (!exists("MooseData")) {
  		assign("MooseData", x, envir=.GlobalEnv)
  } else {
      if (force)
          assign("MooseData", x, envir=.GlobalEnv)
  }
  #assign("MooseData", x, envir=.GlobalEnv)
	invisible(NULL)
}
updateModelTab <- function() {
	ModelTabFull <- model.sel(ModelList, rank = AIC)
  DensityTab <- sapply(names(ModelList), pred_density_moose)
  ModelTab <- data.frame(t(ModelTabFull[,c("AIC","df","logLik","delta")]))
  ModelTab <- rbind(ModelTab, DensityTab[,colnames(ModelTab), drop=FALSE])
	assign("ModelTab", ModelTab, envir=.GlobalEnv)
	invisible(ModelTabFull)
}

## predict total moose abundance and density
pred_total_moose <- function(x, surveyed, fit){
    opts <- getOption("moose_options")
    ## predict response for unsurveyed cells
    pr_uns <- predict(fit, newdata=x[!surveyed,,drop=FALSE], type=c("response"))
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

pred_density_moose <- function(model_id){
    fit <- ModelList[[model_id]]
    opts <- getOption("moose_options")
    x <- MooseData
    srv <- x$srv
    Ntot <- pred_total_moose(x, srv, fit)
    A_all <- sum(x$AREA_KM)
    Density <- Ntot$Ntot_all / A_all
    c(N=Ntot$Ntot_all, A=A_all, D=Density)
}

# note: phi.zi is prob of 1 (not 0 as in zeroinfl)
# poisson gives theta.nb=NULL
rZINB = function(N, mu.nb, theta.nb, phi.zi){
    if (length(phi.zi) < N)
        phi.zi <- rep(phi.zi, N)[seq_len(N)]
    A <- rbinom(N,1,phi.zi)
    if (!is.null(theta.nb)) {
        Z <- rnegbin(N,mu=mu.nb,theta=rep(theta.nb, N))
    } else {
        Z <- rpois(N, lambda=mu.nb)
    }
    Y <- A * Z
    Y
}

MooseSim.PI <-
#function(Survey.data,Unsurvey.data,moose.model,B,model.B,MAXCELL,alpha)
function(model_id, do_boot=TRUE, do_avg=FALSE, dist="negbin")
{
    wt <- updateModelTab()
    if (!any(model_id %in% rownames(wt)))
        stop("model_id not recognized")
    wts <- wt[model_id,,drop=FALSE]
    opts <- getOption("moose_options")
    model_id0 <- model_id
    x <- MooseData
    ## if some survey area is defined, use dual prediction
    DUAL <- !is.null(opts$area_srv)
    if (DUAL) {
        eval(parse(text=paste0("x$area_srv <- x$", opts$area_srv)))
    } else {
        x$area_srv <- TRUE
    }
    x$sort_id <- 1:nrow(x)
    srv <- x$srv
    x$observed_values <- NA
    x$observed_values[srv] <- ModelList[[1]]$y # all models should have same data
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

    pb <- startpb(0, B)
    on.exit(closepb(pb))
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

        if (!(model_id %in% names(ModelList)))
            stop(model_id, " model cannot be found")
        fit <- ModelList[[model_id]]

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
                model.Boot <- try(suppressWarnings(update(fit,
                    data = BSurvey.data,
                    weights=rep(1, nrow(BSurvey.data)),
                    control = zeroinfl.control(
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
                predict.BNS <- predict(model.Boot, newdata=x_uns, type="response")
                predict.BNSout <- if (DUAL && inherits(fit, "wzi")) {
                    predict(model.Boot$unweighted_model,
                        newdata=x_uns, type="response")
                } else {
                    predict.BNS
                }

                if (max(predict.BNS, predict.BNSout) <= MAXCELL && model.Boot$optim$convergence == 0) {
                    Bm.NS <- predict(model.Boot, newdata = x_uns, type="count")
                    Btheta.nb <- model.Boot$theta
                    Bphi.zi <- 1 - predict(model.Boot, newdata = x_uns, type="zero")
                    boot.out[,b] <- rZINB(NS,
                        mu.nb = Bm.NS,
                        theta.nb=Btheta.nb,
                        phi.zi=Bphi.zi) # this os prob of 1 (not 0) and is correct
                    if (DUAL && inherits(fit, "wzi")) {
                        Bm.NSout <- predict(model.Boot$unweighted_model,
                                            newdata = x_uns[!x_uns$area_srv,], type="count")
                        Btheta.nbout <- model.Boot$unweighted_model$theta
                        Bphi.ziout <- 1 - predict(model.Boot$unweighted_model,
                                               newdata = x_uns[!x_uns$area_srv,], type="zero")
                        boot.out[!x_uns$area_srv,b] <- rZINB(sum(!x_uns$area_srv),
                            mu.nb = Bm.NSout,
                            theta.nb=Btheta.nbout,
                            phi.zi=Bphi.ziout)
                    }
                    if (max(boot.out[,b]) <= MAXCELL) {
                        setpb(pb, b)
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
    Cell.PI <- apply(boot.out, 1, quantile, c(alpha/2, 0.5, (1-alpha/2)))
    x_uns$Cell.mean <- rowMeans(boot.out)
    x_uns$Cell.mode <- apply(boot.out, 1, Mode)
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
        model_select_id=mid,
        alpha=alpha,
        boot_full=boot.full[o,,drop=FALSE],
        issues=ISSUES,
#        fit_matrix=if (do_avg) fit.out else NULL, # see fitted_values when NULL
        data=x_full[o,,drop=FALSE])

    csfull <- colSums(boot.full)
    tmPI <- c(Mean=mean(csfull),
        Median=unname(quantile(csfull, 0.5)),
        Mode=Mode(csfull),
        quantile(csfull, c(alpha/2, (1-alpha/2))))
    out$total <- rbind(N=tmPI,
        A=sum(x_full[[opts$Area]]),
        D=tmPI/sum(x_full[[opts$Area]]))
    out
}
## how to get cell.pred with decimals when it is median?
## use mean?


## need to use density to get the model because values tend to
## be spread out
Mode <- function(csfull) {
    d <- density(csfull)
    round(d$x[which.max(d$y)])
}

## Use Unsurveyed.data for plotting/maps
## use ranking and accuracy

savePiData <- function(PI) {
#  tmp <- PI$Unsurvey.data[,c("SU_ID", "Cell.pred", "Cell.PIL",  "Cell.PIU",
#    "Cell.accuracy", "Rank")]
  tmp <- PI$data[!PI$data$srv,,drop=FALSE]
  tmp <- tmp[order(tmp$Cell.accuracy, decreasing=TRUE),]
  assign("PiData", tmp, envir=.GlobalEnv)
	invisible(NULL)
}
## this is used for subset, to avoid confusion
savePiDataSubset <- function(PI) {
  tmp <- PI$data[!PI$data$srv,,drop=FALSE]
  tmp <- tmp[order(tmp$Cell.accuracy, decreasing=TRUE),]
  assign("PiDataSubset", tmp, envir=.GlobalEnv)
    invisible(NULL)
}

subsetPiData <- function(PI, ss) {
    if (missing(ss))
        ss <- rep(TRUE, nrow(PI$data))
    opts <- getOption("moose_options")
    PIout <- PI
    PIout$boot_full <- PI$boot_full[ss,,drop=FALSE]
    PIout$data <- PI$data[ss,,drop=FALSE]
#    PIout$fit_matrix <- NULL
    Rank <- PIout$data$Rank[]
    PIout$data$Rank[!PIout$data$srv] <- as.integer(as.factor(
        rank(-PIout$data$Cell.accuracy[!PIout$data$srv])))

    csfull <- colSums(PIout$boot_full)
    alpha <- getOption("moose_options")$alpha
    tmPI <- c(Mean=mean(csfull),
        Median=unname(quantile(csfull, 0.5)),
        Mode=Mode(csfull),
        quantile(csfull, c(alpha/2, (1-alpha/2))))
    PIout$total <- rbind(N=tmPI,
        A=sum(PIout$data[[opts$Area]]),
        D=tmPI/sum(PIout$data[[opts$Area]]))
    PIout
}

pred_density_moose_PI <- function(PI){
    out <- round(PI$total, 2)
    cat("Total Moose PI summary:\n\n")
    print(out)
    if (length(PI$issues) > 0L) {
        cat("\nNote:", length(PI$issues),
            "issues were found during PI calculations.\n\n")
    }
    invisible(out)
}


plotResiduals <- function(model_id) {
    fit <- ModelList[[model_id]]
    opts <- getOption("moose_options")
    x <- MooseData
    srv <- x$srv
    Y <- fit$y
    Mean <- fit$fitted.values
    z <- (Y - Mean) / sqrt(Y + 0.5)
    xy <- x[!srv, opts$xy]

    Colfun <- colorRampPalette(c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'))
    AbsMax <- max(abs(z))
    Sd <- sd(z)
    nn <- ceiling(AbsMax / Sd)
    br <- seq(-nn*Sd, nn*Sd, by=0.5*Sd)
    tmp <- hist(z, plot=FALSE, breaks=br)
    Col <- Colfun(length(tmp$counts))
    tz <- tanh(z)
    ctz <- cut(z, br)

    op <- par(mfrow=c(1,2))
    plot(x[srv, opts$xy], pch=19, col=Col[ctz], cex=0.5+1.5*abs(tz),
      xlab="Longitude", ylab="Latitude",
      main=paste("Residuals for Model ID:", model_id),
      ylim=range(x[,opts$xy[2]])-c(0.2*diff(range(x[,opts$xy[2]])), 0))
    points(x[!srv,opts$xy], pch="+", col="grey")
    legend("bottomleft", pch=c("o","+"), col="grey",
      bty="n", legend=c("Surveyed", "Unsurveyed"))

    lo <- x[srv,,drop=FALSE]
    lo <- lo[z <= (-1.5*Sd),,drop=FALSE]
    if (nrow(lo) > 0)
        text(lo[, opts$xy], labels = lo$SU_ID, cex=0.8)
    hi <- x[srv,,drop=FALSE]
    hi <- hi[z >= 1.5*Sd,,drop=FALSE]
    if (nrow(hi) > 0)
        text(hi[, opts$xy], labels = hi$SU_ID, cex=0.8)

    hist(z, xlab="Standardized Residuals", col=Col, breaks=br,
      main=paste("Model ID:", model_id))
    par(op)
    invisible(z)
}

plot_predPI <- function(PI) {

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

    Colfun <- colorRampPalette(c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'))
    AbsMax <- max(abs(z))
    Sd <- sd(z)
    nn <- ceiling(AbsMax / Sd)
    br <- seq(-nn*Sd, nn*Sd, by=0.5*Sd)
    tmp <- hist(z, plot=FALSE, breaks=br)
    Col <- Colfun(length(tmp$counts))
    tz <- tanh(z)
    ctz <- cut(z, br)

    op <- par(mfrow=c(1,3))

    ModID <- PI$model_id
    if (length(ModID)>1)
        ModID <- "Avg"
    plot(x[srv, opts$xy], pch=19, col=Col[ctz], cex=0.5+1.5*abs(tz),
        xlab="Longitude", ylab="Latitude",
        main=paste("Residuals for Model ID:", ModID),
        ylim=range(x[,opts$xy[2]])-c(0.2*diff(range(x[,opts$xy[2]])), 0))
    points(x[!srv,opts$xy], pch="+", col="grey")
    legend("bottomleft", pch=c("o","+"), col="grey",
        bty="n", legend=c("Surveyed", "Unsurveyed"))

    lo <- x[srv,,drop=FALSE]
    lo <- lo[z <= (-1.5*Sd),,drop=FALSE]
    if (nrow(lo) > 0)
        text(lo[, opts$xy], labels = lo$SU_ID, cex=0.8)
    hi <- x[srv,,drop=FALSE]
    hi <- hi[z >= 1.5*Sd,,drop=FALSE]
    if (nrow(hi) > 0)
        text(hi[, opts$xy], labels = hi$SU_ID, cex=0.8)

    hist(z, breaks=br, xlab="Standardized Residuals", col=Col,
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

    Colfun <- colorRampPalette(c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026'))
    Col <- Colfun(9)
    #Col <- brewer.pal(9, "YlOrRd")[1:5]
    czAcc <- cut(zAcc, c(-0.1, 0.2, 0.4, 0.6, 0.8, 1.1))

    Min <- sapply(1:20, function(z) length(which(AccRank <= z)))
    Min <- min(20, (1:20)[min(which(Min >= 20))])

    plot(xy, pch=19, col=Col[czAcc], cex=1+zAcc*1,
      ylim=range(x[,opts$xy[2]])-c(0.2*diff(range(x[,opts$xy[2]])), 0),
      xlab="Longitude", ylab="Latitude",
      main=paste("Accuracy for Model ID:", ModID))
    points(x[srv,opts$xy], pch="+", col="grey")
#    text(x[!srv,opts$xy][AccRank <= Min,], labels=AccRank[AccRank <= Min], cex=1)
    text(x[!srv,opts$xy][AccRank <= Min,], labels=x[!srv,"SU_ID"][AccRank <= Min], cex=1)
    legend("bottomleft", pch=19, col=Col[c(5,3,1)],
      bty="n", legend=c("+++", "++", "+"))
    par(op)

    invisible(PI)
}

PlotPiDistr <- function(PI, plot=TRUE, breaks="Sturges") {
    csfull <- colSums(PI$boot_full)
    if (plot) {
        h <- hist(csfull, breaks=breaks, plot=FALSE)
        h$density <- h$counts * 100 / sum(h$counts)
        if (length(unique(csfull)) == 1) {
            h$mids <- unique(csfull)
            h$breaks <- unique(csfull) + c(-0.5, 0.5)
        }
        d <- density(csfull)
        d$y <- max(h$density) * d$y / max(d$y)
        plot(h,
            freq=FALSE, col="lightgrey", main="Total Moose PI",
            xlab="Predicted Total Moose", ylab="Percent",
            border="darkgrey",
            ylim=c(0, max(h$density, d$y)))
        lines(d)
        rug(csfull, col=1)
        abline(v=PI$total["N", "Mean"], col=2)
        abline(v=PI$total["N", "Median"], col=3)
        abline(v=PI$total["N", "Mode"], col=4)
        abline(v=PI$total["N", 4:5], col="darkgrey", lty=2)
        TXT <- paste0(c("Mean", "Median", "Mode"), " = ",
            round(PI$total["N", c("Mean", "Median", "Mode")]))
        legend("topright", lty=c(1,1,1,2), col=c(2:4, "darkgrey"), bty="n",
            legend=c(TXT, paste0(100-100*PI$alpha, "% PI")))
    }
    invisible(csfull)
}
PlotPiDistrCell <- function(PI, id=1, plot=TRUE, breaks="Sturges") {
    csfull <- PI$boot_full[id,]
    if (plot) {
        h <- hist(csfull, breaks=breaks, plot=FALSE)
        h$density <- h$counts * 100 / sum(h$counts)
        if (length(unique(csfull)) == 1) {
            h$mids <- unique(csfull)
            h$breaks <- unique(csfull) + c(-0.5, 0.5)
        }
        d <- density(csfull)
        d$y <- max(h$density) * d$y / max(d$y)
        plot(h,
            freq=FALSE, col="lightgrey", main="Cell Moose PI",
            xlab="Predicted Total Moose in cell", ylab="Percent",
            border="darkgrey",
            ylim=c(0, max(h$density, d$y)))
        lines(d)
        rug(csfull, col=1)
        abline(v=PI$data[id, "Cell.mean"], col=2)
        abline(v=PI$data[id, "Cell.pred"], col=3)
        abline(v=PI$data[id, "Cell.mode"], col=4)
        abline(v=PI$data[id, c("Cell.PIL", "Cell.PIU")], col="darkgrey", lty=2)
        TXT <- paste0(c("Mean", "Median", "Mode"), " = ",
            round(PI$data[id, c("Cell.mean", "Cell.pred", "Cell.mode")]))
        legend("topright", lty=c(1,1,1,2), col=c(2:4, "darkgrey"), bty="n",
            legend=c(TXT, paste0(100-100*PI$alpha, "% PI")))
    }
    invisible(csfull)
}

