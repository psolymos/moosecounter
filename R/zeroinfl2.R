# poisson=ZIP, negbin=ZINB, P=poisson (non-ZI), NB=negbin (non-ZI)
zeroinfl2 <-
function (formula, data, subset, na.action, weights, offset,
    dist = c("poisson", "negbin", "P", "NB"),
    link = c("logit", "probit", "cloglog"),
    control = zeroinfl.control(...),
    model = TRUE, y = TRUE, x = FALSE,
    solveH=TRUE, ...)
{
    ziPoissonNonZI <- function(parms) {
        mu <- as.vector(exp(X %*% parms[1:kx] + offsetx))
        #phi <- as.vector(linkinv(Z %*% parms[(kx + 1):(kx + kz)] + offsetz))
        phi <- linkinv(-100)
        loglik0 <- log(phi + exp(log(1 - phi) - mu))
        loglik1 <- log(1 - phi) + dpois(Y, lambda = mu, log = TRUE)
        loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] *
            loglik1[Y1])
        loglik
    }
    gradPoissonNonZI <- function(parms) {
        eta <- as.vector(X %*% parms[1:kx] + offsetx)
        mu <- exp(eta)
#        etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + offsetz)
        etaz <- -100
        muz <- linkinv(etaz)
        clogdens0 <- -mu
        dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) +
            clogdens0)
        wres_count <- ifelse(Y1, Y - mu, -exp(-log(dens0) + log(1 -
            muz) + clogdens0 + log(mu)))
        wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz),
            (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
        colSums(cbind(wres_count * weights * X, wres_zero * weights *
            Z))
    }
    ziPoisson <- function(parms) {
        mu <- as.vector(exp(X %*% parms[1:kx] + offsetx))
        phi <- as.vector(linkinv(Z %*% parms[(kx + 1):(kx + kz)] +
            offsetz))
        loglik0 <- log(phi + exp(log(1 - phi) - mu))
        loglik1 <- log(1 - phi) + dpois(Y, lambda = mu, log = TRUE)
        loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] *
            loglik1[Y1])
        loglik
    }
    gradPoisson <- function(parms) {
        eta <- as.vector(X %*% parms[1:kx] + offsetx)
        mu <- exp(eta)
        etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + offsetz)
        muz <- linkinv(etaz)
        clogdens0 <- -mu
        dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) +
            clogdens0)
        wres_count <- ifelse(Y1, Y - mu, -exp(-log(dens0) + log(1 -
            muz) + clogdens0 + log(mu)))
        wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz),
            (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
        colSums(cbind(wres_count * weights * X, wres_zero * weights *
            Z))
    }
    ziNegBinNonZI <- function(parms) {
        mu <- as.vector(exp(X %*% parms[1:kx] + offsetx))
        #phi <- as.vector(linkinv(Z %*% parms[(kx + 1):(kx + kz)] + offsetz))
        phi <- linkinv(-100)
        theta <- exp(parms[(kx + kz) + 1])
        loglik0 <- log(phi + exp(log(1 - phi) + suppressWarnings(dnbinom(0,
            size = theta, mu = mu, log = TRUE))))
        loglik1 <- log(1 - phi) + suppressWarnings(dnbinom(Y,
            size = theta, mu = mu, log = TRUE))
        loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] *
            loglik1[Y1])
        loglik
    }
    gradNegBinNonZI <- function(parms) {
        eta <- as.vector(X %*% parms[1:kx] + offsetx)
        mu <- exp(eta)
        #etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + offsetz)
        etaz <- -100
        muz <- linkinv(etaz)
        theta <- exp(parms[(kx + kz) + 1])
        clogdens0 <- dnbinom(0, size = theta, mu = mu, log = TRUE)
        dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) +
            clogdens0)
        wres_count <- ifelse(Y1, Y - mu * (Y + theta)/(mu + theta),
            -exp(-log(dens0) + log(1 - muz) + clogdens0 + log(theta) -
                log(mu + theta) + log(mu)))
        wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz),
            (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
        wres_theta <- theta * ifelse(Y1, digamma(Y + theta) -
            digamma(theta) + log(theta) - log(mu + theta) + 1 -
            (Y + theta)/(mu + theta), exp(-log(dens0) + log(1 -
            muz) + clogdens0) * (log(theta) - log(mu + theta) +
            1 - theta/(mu + theta)))
        colSums(cbind(wres_count * weights * X, wres_zero * weights *
            Z, wres_theta))
    }
    ziNegBin <- function(parms) {
        mu <- as.vector(exp(X %*% parms[1:kx] + offsetx))
        phi <- as.vector(linkinv(Z %*% parms[(kx + 1):(kx + kz)] +
            offsetz))
        theta <- exp(parms[(kx + kz) + 1])
        loglik0 <- log(phi + exp(log(1 - phi) + suppressWarnings(dnbinom(0,
            size = theta, mu = mu, log = TRUE))))
        loglik1 <- log(1 - phi) + suppressWarnings(dnbinom(Y,
            size = theta, mu = mu, log = TRUE))
        loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] *
            loglik1[Y1])
        loglik
    }
    gradNegBin <- function(parms) {
        eta <- as.vector(X %*% parms[1:kx] + offsetx)
        mu <- exp(eta)
        etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + offsetz)
        muz <- linkinv(etaz)
        theta <- exp(parms[(kx + kz) + 1])
        clogdens0 <- dnbinom(0, size = theta, mu = mu, log = TRUE)
        dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) +
            clogdens0)
        wres_count <- ifelse(Y1, Y - mu * (Y + theta)/(mu + theta),
            -exp(-log(dens0) + log(1 - muz) + clogdens0 + log(theta) -
                log(mu + theta) + log(mu)))
        wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz),
            (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
        wres_theta <- theta * ifelse(Y1, digamma(Y + theta) -
            digamma(theta) + log(theta) - log(mu + theta) + 1 -
            (Y + theta)/(mu + theta), exp(-log(dens0) + log(1 -
            muz) + clogdens0) * (log(theta) - log(mu + theta) +
            1 - theta/(mu + theta)))
        colSums(cbind(wres_count * weights * X, wres_zero * weights *
            Z, wres_theta))
    }
    dist <- match.arg(dist)
    NonZI <- dist %in% c("P", "NB")
    loglikfun <- switch(dist,
        poisson = ziPoisson,
        negbin = ziNegBin,
        P = ziPoissonNonZI,
        NB = ziNegBinNonZI)
    gradfun <- switch(dist,
        poisson = gradPoisson,
        negbin = gradNegBin,
        P = gradPoissonNonZI,
        NB = gradNegBinNonZI)
    linkstr <- match.arg(link)
    linkobj <- make.link(linkstr)
    linkinv <- linkobj$linkinv
    cl <- match.call()
    if (missing(data))
        data <- environment(formula)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "weights",
        "offset"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    if (length(formula[[3]]) > 1 && identical(formula[[3]][[1]],
        as.name("|"))) {
        ff <- formula
        formula[[3]][1] <- call("+")
        mf$formula <- formula
        ffc <- . ~ .
        ffz <- ~.
        ffc[[2]] <- ff[[2]]
        ffc[[3]] <- ff[[3]][[2]]
        ffz[[3]] <- ff[[3]][[3]]
        ffz[[2]] <- NULL
    } else {
        ffz <- ffc <- ff <- formula
        ffz[[2]] <- NULL
    }
    if (inherits(try(terms(ffz), silent = TRUE), "try-error")) {
        ffz <- eval(parse(text = sprintf(paste("%s -", deparse(ffc[[2]])),
            deparse(ffz))))
    }
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    mtX <- terms(ffc, data = data)
    X <- model.matrix(mtX, mf)
    mtZ <- terms(ffz, data = data)
    mtZ <- terms(update(mtZ, ~.), data = data)
    Z <- model.matrix(mtZ, mf)
    Y <- model.response(mf, "numeric")
    if (length(Y) < 1)
        stop("empty model")
    if (all(Y > 0))
        stop("invalid dependent variable, minimum count is not zero")
    if (!isTRUE(all.equal(as.vector(Y), as.integer(round(Y +
        0.001)))))
        stop("invalid dependent variable, non-integer values")
    Y <- as.integer(round(Y + 0.001))
    if (any(Y < 0))
        stop("invalid dependent variable, negative counts")
    n <- length(Y)
    kx <- NCOL(X)
    kz <- NCOL(Z)
    Y0 <- Y <= 0
    Y1 <- Y > 0
    weights <- model.weights(mf)
    if (is.null(weights))
        weights <- 1
    if (length(weights) == 1)
        weights <- rep.int(weights, n)
    weights <- as.vector(weights)
    names(weights) <- rownames(mf)
    offsetx <- pscl:::model_offset_2(mf, terms = mtX, offset = TRUE)
    if (is.null(offsetx))
        offsetx <- 0
    if (length(offsetx) == 1)
        offsetx <- rep.int(offsetx, n)
    offsetx <- as.vector(offsetx)
    offsetz <- pscl:::model_offset_2(mf, terms = mtZ, offset = FALSE)
    if (is.null(offsetz))
        offsetz <- 0
    if (length(offsetz) == 1)
        offsetz <- rep.int(offsetz, n)
    offsetz <- as.vector(offsetz)
    start <- control$start
    if (!is.null(start)) {
        valid <- TRUE
        if (!("count" %in% names(start))) {
            valid <- FALSE
            warning("invalid starting values, count model coefficients not specified")
            start$count <- rep.int(0, kx)
        }
        if (!("zero" %in% names(start))) {
            valid <- FALSE
            warning("invalid starting values, zero-inflation model coefficients not specified")
            start$zero <- rep.int(0, kz)
        }
        if (length(start$count) != kx) {
            valid <- FALSE
            warning("invalid starting values, wrong number of count model coefficients")
        }
        if (length(start$zero) != kz) {
            valid <- FALSE
            warning("invalid starting values, wrong number of zero-inflation model coefficients")
        }
        if (dist %in% c("negbin", "NB")) {
            if (!("theta" %in% names(start)))
                start$theta <- 1
            start <- list(count = start$count, zero = start$zero,
                theta = as.vector(start$theta[1]))
        }
        else {
            start <- list(count = start$count, zero = start$zero)
        }
        if (!valid)
            start <- NULL
    }
    if (is.null(start)) {
        if (control$trace)
            cat("generating starting values...")
        model_count <- glm.fit(X, Y, family = poisson(), weights = weights,
            offset = offsetx)
        if (NonZI) {
            start <- list(count = model_count$coefficients,
                zero = rep(-100, kz))
        } else {
            model_zero <- glm.fit(Z, as.integer(Y0), weights = weights,
                family = binomial(link = linkstr), offset = offsetz)
            start <- list(count = model_count$coefficients,
                zero = model_zero$coefficients)
        }
        if (dist %in% c("negbin", "NB"))
            start$theta <- 1
    }
    method <- control$method
    hessian <- control$hessian
    if (!solveH)
        hessian <- FALSE
    ocontrol <- control
    control$method <- control$hessian <- control$EM <- control$start <- NULL
    fit <- optim(fn = loglikfun, gr = gradfun, par = c(start$count,
        start$zero,
        if (dist %in% c("negbin", "NB")) log(start$theta) else NULL),
        method = method, hessian = hessian, control = control)
    if (fit$convergence > 0)
        warning("optimization failed to converge")
    coefc <- fit$par[1:kx]
    names(coefc) <- names(start$count) <- colnames(X)
    coefz <- fit$par[(kx + 1):(kx + kz)]
    names(coefz) <- names(start$zero) <- colnames(Z)

    HM <- if (solveH)
        as.matrix(fit$hessian) else NULL
    if (NonZI) {
        iii <- c(1:kx, if (dist == "NB") kx+kz+1 else NULL)
        tmp <- -solvenear(HM[iii,iii,drop=FALSE])
        vc <- HM
        vc[] <- NA
        vc[iii,iii] <- tmp
    } else {
        vc <- -solvenear(HM)
    }

    if (dist %in% c("negbin", "NB")) {
        np <- kx + kz + 1
        theta <- as.vector(exp(fit$par[np]))
        SE.logtheta <- as.vector(sqrt(diag(vc)[np]))
        vc <- vc[-np, -np, drop = FALSE]
    } else {
        theta <- NULL
        SE.logtheta <- NULL
    }
    colnames(vc) <- rownames(vc) <- c(paste("count", colnames(X),
        sep = "_"), paste("zero", colnames(Z), sep = "_"))
    mu <- exp(X %*% coefc + offsetx)[, 1]
    phi <- if (NonZI)
        linkinv(-100) else linkinv(Z %*% coefz + offsetz)[, 1]
    Yhat <- (1 - phi) * mu
    res <- sqrt(weights) * (Y - Yhat)
    nobs <- sum(weights > 0)
    rval <- list(coefficients = list(count = coefc, zero = coefz),
        residuals = res, fitted.values = Yhat, optim = fit, method = method,
        control = ocontrol, start = start, weights = if (identical(as.vector(weights),
            rep.int(1L, n))) NULL else weights, offset = list(count = if (identical(offsetx,
            rep.int(0, n))) NULL else offsetx, zero = if (identical(offsetz,
            rep.int(0, n))) NULL else offsetz), n = nobs, df.null = nobs -
            2,
        df.residual = nobs - (kx + kz + (dist %in% c("negbin", "NB"))),
        terms = list(count = mtX, zero = mtZ, full = mt), theta = theta,
        SE.logtheta = SE.logtheta, loglik = fit$value, vcov = vc,
        dist = dist, link = linkstr, linkinv = linkinv, converged = fit$convergence <
            1, call = cl, formula = ff, levels = .getXlevels(mt,
            mf), contrasts = list(count = attr(X, "contrasts"),
            zero = attr(Z, "contrasts")))
    if (NonZI) {
        rval$df.residual <- rval$df.residual + kz
    }
    if (model)
        rval$model <- mf
    if (y)
        rval$y <- Y
    if (x)
        rval$x <- list(count = X, zero = Z)
    class(rval) <- if (NonZI)
        c("non_zeroinfl", "zeroinfl") else "zeroinfl"
    return(rval)
}

summary.non_zeroinfl <-
function (object, ...)
{
    tmp <- object
    tmp$dist <- if (object$dist == "NB") "negbin" else "poisson"
    object$residuals <- residuals(tmp, type = "pearson")
    kc <- length(object$coefficients$count)
    kz <- length(object$coefficients$zero)
    se <- sqrt(diag(object$vcov))
    coef <- c(object$coefficients$count, object$coefficients$zero)
    if (object$dist == "NB") {
        coef <- c(coef[1:kc], `Log(theta)` = log(object$theta),
            coef[(kc + 1):(kc + kz)])
        se <- c(se[1:kc], object$SE.logtheta, se[(kc + 1):(kc +
            kz)])
        kc <- kc + 1
    }
    zstat <- coef/se
    pval <- 2 * pnorm(-abs(zstat))
    coef <- cbind(coef, se, zstat, pval)
    colnames(coef) <- c("Estimate", "Std. Error", "z value",
        "Pr(>|z|)")
    object$coefficients$count <- coef[1:kc, , drop = FALSE]
    object$coefficients$zero <- coef[(kc + 1):(kc + kz), , drop = FALSE]
    object$fitted.values <- object$terms <- object$model <- object$y <- object$x <- object$levels <- object$contrasts <- object$start <- NULL
    class(object) <- "summary.non_zeroinfl"
    object
}

print.summary.non_zeroinfl <-
function (x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:", deparse(x$call, width.cutoff = floor(getOption("width") *
        0.85)), "", sep = "\n")
    if (!x$converged) {
        cat("model did not converge\n")
    } else {
        cat("Pearson residuals:\n")
        print(structure(quantile(x$residuals), names = c("Min",
            "1Q", "Median", "3Q", "Max")), digits = digits, ...)
        cat(paste("\nCount model coefficients (", x$dist, " with log link):\n",
            sep = ""))
        printCoefmat(x$coefficients$count, digits = digits, signif.legend = FALSE)
        #cat(paste("\nZero-inflation model coefficients (binomial with ",
        #    x$link, " link):\n", sep = ""))
        #printCoefmat(x$coefficients$zero, digits = digits, signif.legend = FALSE)
        if (getOption("show.signif.stars") & any(x$coefficients$count[, 4] < 0.1))
            cat("---\nSignif. codes: ", "0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
                "\n")
        if (x$dist == "NB")
            cat(paste("\nTheta =", round(x$theta, digits), "\n"))
        else cat("\n")
        cat(paste("Number of iterations in", x$method, "optimization:",
            tail(na.omit(x$optim$count), 1), "\n"))
        cat("Log-likelihood:", formatC(x$loglik, digits = digits),
            "on", x$n - x$df.residual, "Df\n")
    }
    invisible(x)
}

nobs.zeroinfl <- function(object, ...) object$n
