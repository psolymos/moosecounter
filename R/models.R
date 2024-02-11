#' Internal Functions: Models
#'
#' These functions power the total Moose estimation and prediction.
#'
#' `zeroinfl2` is a customized version of the `pscl::zeroinfl()` function,
#' but this also fits the non-ZI counterparts in a way that simplifies
#' downstream analyses (i.e. PI calculations). Intended for internal use.
#'
#' `wzi` applies a leave-one-out approach to temper influential observations.
#' The process finds weights that are related to leverage
#' (how much each observation contributes to the model likelihood).
#'
#' The robust version (CL/PL = conditional likelihood / pseudo likelihood)
#' applies the conditioning described in Solymos et al. (2021)
#' <https://doi.org/10.1002/env.1149>.
#'
#' @param formula Model formula as in `pscl::zeroinfl()`.
#' @param data Moose data frame.
#' @param subset,na.action,weights,offset,model,y Arguments as
#'   in `pscl::zeroinfl()`.
#' @param control See `pscl::zeroinfl.control()`.
#' @param solveH Logical, to use robust matrix inversion to get VCV.
#' @param robust Logical, use CL/PL robust regression approach.
#' @param dist Count distribution, one of `"ZIP"`, `"ZINB"`, `"P"`, `"NB"`.
#' @param link Link function for the zero model.
#' @param object A model as returned by `zeroinfl2()`.
#' @param pass_data Logical, to pass the data or not.
#' @param x  Arguments for `pscl::zeroinfl()` or a
#'   model as returned by `zeroinfl2()` for the methods.
#' @param digits Digits for print method.
#' @param ... Other parameters passes to underlying functions.
#'
#' @name models
#' @keywords internal models regression
NULL

#' @rdname models
#' @export
## weighted ZI model to tame influential observations
wzi <- function(object, pass_data=FALSE, ...) {
    wscale <- getOption("moose_options")$wscale
    n <- stats::nobs(object)
    ll0 <- as.numeric(stats::logLik(object))
    ll <- numeric(n)
    w <- rep(1, n)
    ctrl <- pscl::zeroinfl.control(
        method = getOption("moose_options")$method,
        start = attr(object, "parms.start"))
    d <- stats::model.frame(object)
    Form <- stats::as.formula(object$chrformula)
    for (i in seq_len(n)) {
        m <- try(suppressWarnings(stats::update(
            object, 
            data=d[-i,,drop=FALSE],
            weights=w[-i], 
            control=ctrl,
            robust = object$robust,
            dist = object$dist)), silent=FALSE)
        ll[i] <- if (inherits(m, "try-error"))
            (n-1)*ll0/n else as.numeric(stats::logLik(m))
    }
    w <- 1/abs(ll0-ll)^wscale
    w <- n*w/sum(w)
    if (pass_data) {
        out <- try(suppressWarnings(stats::update(object, data=d,
            weights=w, control=ctrl)), silent=TRUE)
        if (inherits(out, "try-error"))
            out <- object
    } else {
        out <- try(suppressWarnings(stats::update(object,
            weights=w, control=ctrl)), silent=TRUE)
        if (inherits(out, "try-error"))
            out <- object
    }
    out$unweighted_model <- object
    class(out) <- c("wzi", class(out))
    out
}

#' @rdname models
#' @export
## calculate leave-one-out error as blended Chi-square distance
loo <- function(object, ...) {
    if (is.null(object$y))
        stop("Please use y=TRUE when fitting the model object.")
    n <- stats::nobs(object)
    xv <- rep(NA_real_, n)
    ctrl <- pscl::zeroinfl.control(
        method = getOption("moose_options")$method,
        start = attr(object, "parms.start"))
    d <- stats::model.frame(object)
    Form <- stats::as.formula(object$chrformula)
    for (i in seq_len(n)) {
        print(object$call)
        m <- try(suppressWarnings(stats::update(
            object, 
            data=d[-i,,drop=FALSE], 
            control=ctrl,
            robust = object$robust,
            dist = object$dist)), silent=FALSE)
        if (!inherits(m, "try-error")) {
            pr <- stats::predict(m, newdata = d[i,,drop=FALSE], type = "response")
            xv[i] <- (pr - object$y[i])^2 / (0.5*pr + 0.5*object$y[i])
        }
    }
    object$xv <- xv
    object
}

#' @rdname models
#' @export
#' @importFrom stats .getXlevels binomial dnbinom dpois glm.fit make.link model.matrix model.response model.weights optim pnbinom poisson ppois terms update
# poisson=ZIP, negbin=ZINB, P=poisson (non-ZI), NB=negbin (non-ZI)
zeroinfl2 <- function (formula, data,
    subset, na.action, weights, offset,
    dist = "ZIP",
    link = c("logit", "probit", "cloglog"),
    control = NULL,
    model = TRUE, y = TRUE, x = FALSE,
    solveH=TRUE, robust=FALSE, ...) {

    dist0 <- dist
    dist <- switch(dist,
        "P"="P",
        "NB"="NB",
        "ZIP"="poisson",
        "ZINB"="negbin",
        "HP"="poisson",
        "HNB"="negbin",
        "poisson"="poisson",
        "negbin"="negbin",
        stop("dist must be one of P, NB, HP, HNB, ZIP, or ZINB"))

    hurdle <- dist0 %in% c("HP", "HNB")
    if (is.null(control)) {
        control <- if (hurdle)
            pscl::hurdle.control(...) else pscl::zeroinfl.control(...)
    }

    .model_offset_2 <- function (x, terms = NULL, offset = TRUE) {
        if (is.null(terms))
            terms <- attr(x, "terms")
        offsets <- attr(terms, "offset")
        if (length(offsets) > 0) {
            ans <- if (offset)
                x$"(offset)"
            else NULL
            if (is.null(ans))
                ans <- 0
            for (i in offsets) ans <- ans + x[[deparse(attr(terms,
                "variables")[[i + 1]])]]
            ans
        }
        else {
            ans <- if (offset)
                x$"(offset)"
            else NULL
        }
        if (!is.null(ans) && !is.numeric(ans))
            stop("'offset' must be numeric")
        ans
    }

    if (hurdle) {
        zeroPoisson <- function(parms) {
            mu <- as.vector(exp(Z %*% parms + offsetz))
            loglik0 <- -mu
            loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] * 
                log(1 - exp(loglik0[Y1])))
            loglik
        }
        countPoisson <- function(parms) {
            mu <- as.vector(exp(X %*% parms + offsetx))[Y1]
            loglik0 <- -mu
            loglik1 <- dpois(Y[Y1], lambda = mu, log = TRUE)
            loglik <- sum(weights[Y1] * loglik1) - sum(weights[Y1] * 
                log(1 - exp(loglik0)))
            loglik
        }
        zeroNegBin <- function(parms) {
            mu <- as.vector(exp(Z %*% parms[1:kz] + offsetz))
            theta <- exp(parms[kz + 1])
            loglik0 <- suppressWarnings(dnbinom(0, size = theta, 
                mu = mu, log = TRUE))
            loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] * 
                log(1 - exp(loglik0[Y1])))
            loglik
        }
        countNegBin <- function(parms) {
            mu <- as.vector(exp(X %*% parms[1:kx] + offsetx))[Y1]
            theta <- exp(parms[kx + 1])
            loglik0 <- suppressWarnings(dnbinom(0, size = theta, 
                mu = mu, log = TRUE))
            loglik1 <- suppressWarnings(dnbinom(Y[Y1], size = theta, 
                mu = mu, log = TRUE))
            loglik <- sum(weights[Y1] * loglik1) - sum(weights[Y1] * 
                log(1 - exp(loglik0)))
            loglik
        }
        zeroGeom <- function(parms) zeroNegBin(c(parms, 0))
        countGeom <- function(parms) countNegBin(c(parms, 0))
        zeroBinom <- function(parms) {
            mu <- as.vector(linkinv(Z %*% parms + offsetz))
            loglik <- sum(weights[Y0] * log(1 - mu[Y0])) + sum(weights[Y1] * 
                log(mu[Y1]))
            loglik
        }
        countGradPoisson <- function(parms) {
            eta <- as.vector(X %*% parms + offsetx)[Y1]
            mu <- exp(eta)
            colSums(((Y[Y1] - mu) - exp(ppois(0, lambda = mu, log.p = TRUE) - 
                ppois(0, lambda = mu, lower.tail = FALSE, log.p = TRUE) + 
                eta)) * weights[Y1] * X[Y1, , drop = FALSE])
        }
        countGradGeom <- function(parms) {
            eta <- as.vector(X %*% parms + offsetx)[Y1]
            mu <- exp(eta)
            colSums(((Y[Y1] - mu * (Y[Y1] + 1)/(mu + 1)) - exp(pnbinom(0, 
                mu = mu, size = 1, log.p = TRUE) - pnbinom(0, mu = mu, 
                size = 1, lower.tail = FALSE, log.p = TRUE) - log(mu + 
                1) + eta)) * weights[Y1] * X[Y1, , drop = FALSE])
        }
        countGradNegBin <- function(parms) {
            eta <- as.vector(X %*% parms[1:kx] + offsetx)[Y1]
            mu <- exp(eta)
            theta <- exp(parms[kx + 1])
            logratio <- pnbinom(0, mu = mu, size = theta, log.p = TRUE) - 
                pnbinom(0, mu = mu, size = theta, lower.tail = FALSE, 
                    log.p = TRUE)
            rval <- colSums(((Y[Y1] - mu * (Y[Y1] + theta)/(mu + 
                theta)) - exp(logratio + log(theta) - log(mu + theta) + 
                eta)) * weights[Y1] * X[Y1, , drop = FALSE])
            rval2 <- sum((digamma(Y[Y1] + theta) - digamma(theta) + 
                log(theta) - log(mu + theta) + 1 - (Y[Y1] + theta)/(mu + 
                theta) + exp(logratio) * (log(theta) - log(mu + theta) + 
                1 - theta/(mu + theta))) * weights[Y1]) * theta
            c(rval, rval2)
        }
        zeroGradPoisson <- function(parms) {
            eta <- as.vector(Z %*% parms + offsetz)
            mu <- exp(eta)
            colSums(ifelse(Y0, -mu, exp(ppois(0, lambda = mu, log.p = TRUE) - 
                ppois(0, lambda = mu, lower.tail = FALSE, log.p = TRUE) + 
                eta)) * weights * Z)
        }
        zeroGradGeom <- function(parms) {
            eta <- as.vector(Z %*% parms + offsetz)
            mu <- exp(eta)
            colSums(ifelse(Y0, -mu/(mu + 1), exp(pnbinom(0, mu = mu, 
                size = 1, log.p = TRUE) - pnbinom(0, mu = mu, size = 1, 
                lower.tail = FALSE, log.p = TRUE) - log(mu + 1) + 
                eta)) * weights * Z)
        }
        zeroGradNegBin <- function(parms) {
            eta <- as.vector(Z %*% parms[1:kz] + offsetz)
            mu <- exp(eta)
            theta <- exp(parms[kz + 1])
            logratio <- pnbinom(0, mu = mu, size = theta, log.p = TRUE) - 
                pnbinom(0, mu = mu, size = theta, lower.tail = FALSE, 
                    log.p = TRUE)
            rval <- colSums(ifelse(Y0, -mu * theta/(mu + theta), 
                exp(logratio + log(theta) - log(mu + theta) + eta)) * 
                weights * Z)
            rval2 <- sum(ifelse(Y0, log(theta) - log(mu + theta) + 
                1 - theta/(mu + theta), -exp(logratio) * (log(theta) - 
                log(mu + theta) + 1 - theta/(mu + theta))) * weights * 
                theta)
            c(rval, rval2)
        }
        zeroGradBinom <- function(parms) {
            eta <- as.vector(Z %*% parms + offsetz)
            mu <- linkinv(eta)
            colSums(ifelse(Y0, -1/(1 - mu), 1/mu) * linkobj$mu.eta(eta) * 
                weights * Z)
        }
        # dist <- match.arg(dist)
        # zero.dist <- match.arg(zero.dist)
        zero.dist <- "binomial"
        countDist <- switch(dist, poisson = countPoisson, geometric = countGeom, 
            negbin = countNegBin)
        zeroDist <- switch(zero.dist, poisson = zeroPoisson, geometric = zeroGeom, 
            negbin = zeroNegBin, binomial = zeroBinom)
        countGrad <- switch(dist, poisson = countGradPoisson, geometric = countGradGeom, 
            negbin = countGradNegBin)
        zeroGrad <- switch(zero.dist, poisson = zeroGradPoisson, 
            geometric = zeroGradGeom, negbin = zeroGradNegBin, binomial = zeroGradBinom)
        loglikfun <- function(parms) countDist(parms[1:(kx + (dist == 
            "negbin"))]) + zeroDist(parms[(kx + (dist == "negbin") + 
            1):(kx + kz + (dist == "negbin") + (zero.dist == "negbin"))])
        gradfun <- function(parms) c(countGrad(parms[1:(kx + (dist == 
            "negbin"))]), zeroGrad(parms[(kx + (dist == "negbin") + 
            1):(kx + kz + (dist == "negbin") + (zero.dist == "negbin"))]))
        linkstr <- match.arg(link)
        linkobj <- make.link(linkstr)
        linkinv <- linkobj$linkinv
        if (control$trace) 
            cat("Hurdle Count Model\n", paste("count model:", dist, 
                "with log link\n"), paste("zero hurdle model:", zero.dist, 
                "with", ifelse(zero.dist == "binomial", linkstr, 
                    "log"), "link\n"), sep = "")
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
        }
        else {
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
        if (zero.dist == "negbin" & isTRUE(all.equal(as.vector(Z), 
            rep.int(Z[1], length(Z))))) 
            stop("negative binomial zero hurdle model is not identified with only an intercept")
        if (control$trace) {
            cat("dependent variable:\n")
            tab <- table(factor(Y, levels = 0:max(Y)), exclude = NULL)
            names(dimnames(tab)) <- NULL
            print(tab)
        }
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
        offsetx <- .model_offset_2(mf, terms = mtX, offset = TRUE)
        if (is.null(offsetx)) 
            offsetx <- 0
        if (length(offsetx) == 1) 
            offsetx <- rep.int(offsetx, n)
        offsetx <- as.vector(offsetx)
        offsetz <- .model_offset_2(mf, terms = mtZ, offset = FALSE)
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
            if (dist == "negbin" | zero.dist == "negbin") {
                if (!("theta" %in% names(start))) 
                    start$theta <- c(1, 1)
                start <- list(count = start$count, zero = start$zero, 
                    theta = rep(start$theta, length.out = 2))
                if (is.null(names(start$theta))) 
                    names(start$theta) <- c("count", "zero")
                if (dist != "negbin") 
                    start$theta <- start$theta["zero"]
                if (zero.dist != "negbin") 
                    start$theta <- start$theta["count"]
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
            model_zero <- switch(zero.dist, poisson = glm.fit(Z, 
                Y, family = poisson(), weights = weights, offset = offsetz), 
                negbin = glm.fit(Z, Y, family = poisson(), weights = weights, 
                    offset = offsetz), geometric = suppressWarnings(glm.fit(Z, 
                    factor(Y > 0), family = binomial(), weights = weights, 
                    offset = offsetz)), binomial = suppressWarnings(glm.fit(Z, 
                    factor(Y > 0), family = binomial(link = linkstr), 
                    weights = weights, offset = offsetz)))
            start <- list(count = model_count$coefficients, zero = model_zero$coefficients)
            start$theta <- c(count = if (dist == "negbin") 1 else NULL, 
                zero = if (zero.dist == "negbin") 1 else NULL)
            if (control$trace) 
                cat("done\n")
        }
        method <- control$method
        hessian <- control$hessian
        if (!solveH)
            hessian <- FALSE
        separate <- control$separate
        control$method <- control$hessian <- control$separate <- control$start <- NULL
        if (separate) {
            if (control$trace) 
                cat("calling optim() for count component estimation:\n")
            fit_count <- optim(fn = countDist, gr = countGrad, par = c(start$count, 
                if (dist == "negbin") log(start$theta["count"]) else NULL), 
                method = method, hessian = hessian, control = control)
            if (control$trace) 
                cat("calling optim() for zero hurdle component estimation:\n")
            fit_zero <- optim(fn = zeroDist, gr = zeroGrad, par = c(start$zero, 
                if (zero.dist == "negbin") log(start$theta["zero"]) else NULL), 
                method = method, hessian = hessian, control = control)
            if (control$trace) 
                cat("done\n")
            fit <- list(count = fit_count, zero = fit_zero)
            coefc <- fit_count$par[1:kx]
            coefz <- fit_zero$par[1:kz]
            theta <- c(count = if (dist == "negbin") as.vector(exp(fit_count$par[kx + 
                1])) else NULL, zero = if (zero.dist == "negbin") as.vector(exp(fit_zero$par[kz + 
                1])) else NULL)
            vc_count <- tryCatch(-solve(as.matrix(fit_count$hessian)), 
                error = function(e) {
                    warning(e$message, call = FALSE)
                    k <- nrow(as.matrix(fit_count$hessian))
                    return(matrix(NA, k, k))
                })
            vc_zero <- tryCatch(-solve(as.matrix(fit_zero$hessian)), 
                error = function(e) {
                    warning(e$message, call = FALSE)
                    k <- nrow(as.matrix(fit_zero$hessian))
                    return(matrix(NA, k, k))
                })
            SE.logtheta <- list()
            if (dist == "negbin") {
                SE.logtheta$count <- as.vector(sqrt(diag(vc_count)[kx + 
                    1]))
                vc_count <- vc_count[-(kx + 1), -(kx + 1), drop = FALSE]
            }
            if (zero.dist == "negbin") {
                SE.logtheta$zero <- as.vector(sqrt(diag(vc_zero)[kz + 
                    1]))
                vc_zero <- vc_zero[-(kz + 1), -(kz + 1), drop = FALSE]
            }
            vc <- rbind(cbind(vc_count, matrix(0, kx, kz)), cbind(matrix(0, 
                kz, kx), vc_zero))
            SE.logtheta <- unlist(SE.logtheta)
        }
        else {
            if (control$trace) 
                cat("calling optim() for joint count and zero hurlde estimation:\n")
            fit <- optim(fn = loglikfun, gr = gradfun, par = c(start$count, 
                if (dist == "negbin") log(start$theta["count"]) else NULL, 
                start$zero, if (zero.dist == "negbin") log(start$theta["zero"]) else NULL), 
                method = method, hessian = hessian, control = control)
            if (fit$convergence > 0) 
                warning("optimization failed to converge")
            if (control$trace) 
                cat("done\n")
            coefc <- fit$par[1:kx]
            coefz <- fit$par[(kx + (dist == "negbin") + 1):(kx + 
                kz + (dist == "negbin"))]
            vc <- tryCatch(-solve(as.matrix(fit$hessian)), error = function(e) {
                warning(e$message, call = FALSE)
                k <- nrow(as.matrix(fit$hessian))
                return(matrix(NA, k, k))
            })
            np <- c(if (dist == "negbin") kx + 1 else NULL, if (zero.dist == 
                "negbin") kx + kz + 1 + (dist == "negbin") else NULL)
            if (length(np) > 0) {
                theta <- as.vector(exp(fit$par[np]))
                SE.logtheta <- as.vector(sqrt(diag(vc)[np]))
                names(theta) <- names(SE.logtheta) <- c(if (dist == 
                    "negbin") "count" else NULL, if (zero.dist == 
                    "negbin") "zero" else NULL)
                vc <- vc[-np, -np, drop = FALSE]
            }
            else {
                theta <- NULL
                SE.logtheta <- NULL
            }
        }
        names(coefc) <- names(start$count) <- colnames(X)
        names(coefz) <- names(start$zero) <- colnames(Z)
        colnames(vc) <- rownames(vc) <- c(paste("count", colnames(X), 
            sep = "_"), paste("zero", colnames(Z), sep = "_"))
        phi <- if (zero.dist == "binomial") 
            linkinv(Z %*% coefz + offsetz)[, 1]
        else exp(Z %*% coefz + offsetz)[, 1]
        p0_zero <- switch(zero.dist, binomial = log(phi), poisson = ppois(0, 
            lambda = phi, lower.tail = FALSE, log.p = TRUE), negbin = pnbinom(0, 
            size = theta["zero"], mu = phi, lower.tail = FALSE, log.p = TRUE), 
            geometric = pnbinom(0, size = 1, mu = phi, lower.tail = FALSE, 
                log.p = TRUE))
        mu <- exp(X %*% coefc + offsetx)[, 1]
        p0_count <- switch(dist, poisson = ppois(0, lambda = mu, 
            lower.tail = FALSE, log.p = TRUE), negbin = pnbinom(0, 
            size = theta["count"], mu = mu, lower.tail = FALSE, log.p = TRUE), 
            geometric = pnbinom(0, size = 1, mu = mu, lower.tail = FALSE, 
                log.p = TRUE))
        Yhat <- exp((p0_zero - p0_count) + log(mu))
        res <- sqrt(weights) * (Y - Yhat)
        nobs <- sum(weights > 0)
        rval <- list(coefficients = list(count = coefc, zero = coefz), 
            residuals = res, fitted.values = Yhat, optim = fit, method = method, 
            control = control, start = start, weights = if (identical(as.vector(weights), 
                rep.int(1L, n))) NULL else weights, offset = list(count = if (identical(offsetx, 
                rep.int(0, n))) NULL else offsetx, zero = if (identical(offsetz, 
                rep.int(0, n))) NULL else offsetz), n = nobs, df.null = nobs - 
                2, df.residual = nobs - (kx + kz + (dist == "negbin") + 
                (zero.dist == "negbin")), terms = list(count = mtX, 
                zero = mtZ, full = mt), theta = theta, SE.logtheta = SE.logtheta, 
            loglik = if (separate) fit_count$value + fit_zero$value else fit$value, 
            vcov = vc, dist = list(count = dist, zero = zero.dist), 
            link = if (zero.dist == "binomial") linkstr else NULL, 
            linkinv = if (zero.dist == "binomial") linkinv else NULL, 
            separate = separate, converged = if (separate) fit_count$convergence < 
                1 & fit_zero$convergence < 1 else fit$convergence < 
                1, call = cl, formula = ff, levels = .getXlevels(mt, 
                mf), contrasts = list(count = attr(X, "contrasts"), 
                zero = attr(Z, "contrasts")))
        if (model) 
            rval$model <- mf
        if (y) 
            rval$y <- Y
        if (x) 
            rval$x <- list(count = X, zero = Z)
        class(rval) <- "hurdle"
        return(rval)

    } else {
        ziPoissonNonZI <- function(parms) {
            mu <- as.vector(exp(X %*% parms[1:kx] + offsetx))
            #phi <- as.vector(linkinv(Z %*% parms[(kx + 1):(kx + kz)] + offsetz))
            phi <- linkinv(-100)
            loglik0 <- log(phi + exp(log(1 - phi) - mu))
            loglik1 <- log(1 - phi) + stats::dpois(Y, lambda = mu, log = TRUE)
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
            loglik1 <- log(1 - phi) + stats::dpois(Y, lambda = mu, log = TRUE)
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
            loglik0 <- log(phi + exp(log(1 - phi) + suppressWarnings(stats::dnbinom(0,
                size = theta, mu = mu, log = TRUE))))
            loglik1 <- log(1 - phi) + suppressWarnings(stats::dnbinom(Y,
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
            clogdens0 <- stats::dnbinom(0, size = theta, mu = mu, log = TRUE)
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
            loglik0 <- log(phi + exp(log(1 - phi) + suppressWarnings(stats::dnbinom(0,
                size = theta, mu = mu, log = TRUE))))
            loglik1 <- log(1 - phi) + suppressWarnings(stats::dnbinom(Y,
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
            clogdens0 <- stats::dnbinom(0, size = theta, mu = mu, log = TRUE)
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
        #dist <- match.arg(dist)
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
        linkobj <- stats::make.link(linkstr)
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
        if (inherits(try(stats::terms(ffz), silent = TRUE), "try-error")) {
            ffz <- eval(parse(text = sprintf(paste("%s -", deparse(ffc[[2]])),
                deparse(ffz))))
        }
        mf[[1]] <- as.name("model.frame")
        mf <- eval(mf, parent.frame())
        mt <- attr(mf, "terms")
        mtX <- stats::terms(ffc, data = data)
        X <- stats::model.matrix(mtX, mf)
        mtZ <- stats::terms(ffz, data = data)
        mtZ <- stats::terms(stats::update(mtZ, ~.), data = data)
        Z <- stats::model.matrix(mtZ, mf)
        Y <- stats::model.response(mf, "numeric")
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
        weights <- stats::model.weights(mf)
        if (is.null(weights))
            weights <- 1
        if (length(weights) == 1)
            weights <- rep.int(weights, n)
        weights <- as.vector(weights)
        names(weights) <- rownames(mf)

        # if (hurdle) {
        #     out <- pscl::hurdle(formula=formula, data=data, 
        #         # subset=subset, 
        #         # na.action=na.action, 
        #         # weights=weights, 
        #         # offset=offset,
        #         dist = dist, zero.dist = "binomial",
        #         link = match.arg(link),
        #         control = control,
        #         model = model, y = y, x = x, ...)
        #     return(out)
        # }

        offsetx <- .model_offset_2(mf, terms = mtX, offset = TRUE)
        if (is.null(offsetx))
            offsetx <- 0
        if (length(offsetx) == 1)
            offsetx <- rep.int(offsetx, n)
        offsetx <- as.vector(offsetx)
        offsetz <- .model_offset_2(mf, terms = mtZ, offset = FALSE)
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
            model_count <- stats::glm.fit(X, Y,
                family = stats::poisson(), weights = weights,
                offset = offsetx)
            if (NonZI) {
                start <- list(count = model_count$coefficients,
                    zero = rep(-100, kz))
            } else {
                model_zero <- stats::glm.fit(Z, as.integer(Y0), weights = weights,
                    family = stats::binomial(link = linkstr), offset = offsetz)
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

        if (robust) {
            fit <- .zeroinfl_clpl(Y=Y, X=X, Z=Z, 
                offsetx=offsetx, offsetz=offsetz, weights=weights,
                distr=dist0, link=linkstr,
                method=method, 
                inits=c(start$count,
                    if (dist %in% c("negbin", "NB")) log(start$theta) else NULL,
                    start$zero), 
                control=control, hessian=hessian)
            if (dist0 == "P") {
                fit$par <- c(fit$par[1:kx], rep(-100,kz))
                if (hessian) {
                    hess <- fit$hessian
                    fit$hessian <- matrix(NA, kx + kz, kx + kz)
                    fit$hessian[1:kx,1:kx] <- hess
                }
            }
            if (dist0 == "NB") {
                fit$par <- c(fit$par[1:kx], rep(-100,kz), fit$par[kx+1])
                if (hessian) {
                    hess <- fit$hessian
                    fit$hessian <- matrix(NA, kx + kz + 1, kx + kz + 1)
                    iii <- c(1:kx, if (dist == "NB") kx+kz+1 else NULL)
                    fit$hessian[iii,iii] <- hess
                }
            }

        } else {
            fit <- stats::optim(fn = loglikfun, gr = gradfun, par = c(start$count,
                start$zero,
                if (dist %in% c("negbin", "NB")) log(start$theta) else NULL),
                method = method, hessian = hessian, control = control)
        }

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
            residuals = res, fitted.values = Yhat, 
            optim = fit, robust = robust,
            method = method,
            control = ocontrol, start = start, weights = if (identical(as.vector(weights),
                rep.int(1L, n))) NULL else weights, offset = list(count = if (identical(offsetx,
                rep.int(0, n))) NULL else offsetx, zero = if (identical(offsetz,
                rep.int(0, n))) NULL else offsetz), n = nobs, df.null = nobs -
                2,
            df.residual = nobs - (kx + kz + (dist %in% c("negbin", "NB"))),
            terms = list(count = mtX, zero = mtZ, full = mt), theta = theta,
            SE.logtheta = SE.logtheta, loglik = fit$value, vcov = vc,
            dist = dist, link = linkstr, linkinv = linkinv, converged = fit$convergence <
                1, call = cl, formula = ff, levels = stats::.getXlevels(mt,
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
}

#' @rdname models
#' @export
summary.non_zeroinfl <-
function (object, ...)
{
    tmp <- object
    tmp$dist <- if (object$dist == "NB") "negbin" else "poisson"
    object$residuals <- stats::residuals(tmp, type = "pearson")
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
    pval <- 2 * stats::pnorm(-abs(zstat))
    coef <- cbind(coef, se, zstat, pval)
    colnames(coef) <- c("Estimate", "Std. Error", "z value",
        "Pr(>|z|)")
    object$coefficients$count <- coef[1:kc, , drop = FALSE]
    object$coefficients$zero <- coef[(kc + 1):(kc + kz), , drop = FALSE]
    object$fitted.values <- object$terms <- object$model <- object$y <- object$x <- object$levels <- object$contrasts <- object$start <- NULL
    class(object) <- "summary.non_zeroinfl"
    object
}

#' @rdname models
#' @export
print.summary.non_zeroinfl <-
function (x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:", deparse(x$call, width.cutoff = floor(getOption("width") *
        0.85)), "", sep = "\n")
    if (!x$converged) {
        cat("model did not converge\n")
    } else {
        cat("Pearson residuals:\n")
        print(structure(stats::quantile(x$residuals), names = c("Min",
            "1Q", "Median", "3Q", "Max")), digits = digits, ...)
        cat(paste("\nCount model coefficients (", x$dist, " with log link):\n",
            sep = ""))
        stats::printCoefmat(
            x$coefficients$count, digits = digits, signif.legend = FALSE)
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
            utils::tail(stats::na.omit(x$optim$count), 1), "\n"))
        cat("Log-likelihood:", formatC(x$loglik, digits = digits),
            "on", x$n - x$df.residual, "Df\n")
    }
    invisible(x)
}

#' @rdname models
#' @importFrom stats nobs
#' @export
nobs.zeroinfl <- function(object, ...) object$n


#' Internal function for CL/PL robust fitting
#' 
#' @param Y,X,Z Response vector and count, zero model matrices.
#' @param offsetx,offsetz,weights Offsets and weights.
#' @param distr,link Distribution and zero link function.
#' @param method,inits,control,hessian,... Parameters passed to `optin()`.
#' 
#' @noRd
.zeroinfl_clpl <- function(Y, X, Z, offsetx, offsetz, weights,
distr=c("P", "NB", "ZIP", "ZINB"), link="logit",
method="Nelder-Mead", inits=NULL, control=list(), hessian=TRUE,  ...){

    distr <- match.arg(distr)
    zi <- distr %in% c("ZIP", "ZINB")
    distr <- if (distr %in% c("P", "ZIP"))
        "poisson" else "negbin"

    ## >>> ZI-POIS
    ## linkinvx=exp
    nll_P_ML <- function(parms) {
        mu <- as.vector(linkinvx(X %*% parms[1:kx] + offsetx))
        loglik <- sum(weights * stats::dpois(Y, mu, log = TRUE))
        if (!is.finite(loglik) || is.na(loglik))
            loglik <- -good.num.limit[2]
        loglik
    }
    nll_ZIP_ML <- function(parms) {
        mu <- as.vector(linkinvx(X %*% parms[1:kx] + offsetx))
        phi <- as.vector(linkinvz(Z %*% parms[(kx + 1):(kx + kz)] + offsetz))
        loglik0 <- log(phi + exp(log(1 - phi) - mu))
        loglik1 <- log(1 - phi) + stats::dpois(Y, lambda = mu, log = TRUE)
        loglik <- sum(weights[id0] * loglik0[id0]) + sum(weights[id1] *
            loglik1[id1])
        if (!is.finite(loglik) || is.na(loglik))
            loglik <- -good.num.limit[2]
        loglik
    }
    nll_ZIP_CL <- function(parms) {
        mu1 <- as.vector(linkinvx(X1 %*% parms[1:kx] + offsetx1))
        num <- stats::dpois(Y1, mu1, log = TRUE)
        den <- log(1 - exp(-mu1))
        loglik <- sum(weights1 * (num - den))
        if (!is.finite(loglik) || is.na(loglik))
            loglik <- -good.num.limit[2]
        loglik
    }
    logd0_ZIP <- function(parms) {
        -as.vector(linkinvx(X %*% parms[1:kx] + offsetx))
    }
    ## this function applies to all distributions
    ## because logd0 (log density for 0 obs) is plug-in
    ## logd0 needs to incorporate offsets but not weights
    nll_ZIP_PL <- function(parms, logd0) {
        phi <- as.vector(linkinvz(Z %*% parms + offsetz))
        loglik0 <- log(phi + exp(log(1 - phi) + logd0))
        loglik1 <- log(1 - phi) + log(1 - exp(logd0))
        loglik <- sum(weights * ifelse(Y==0, loglik0, loglik1))
        if (!is.finite(loglik) || is.na(loglik))
            loglik <- -good.num.limit[2]
        loglik
    }

    ## >>> ZI-NEGBIN
    ## linkinvx=exp
    ## theta is precision
    nll_NB_ML <- function(parms) {
        mu <- as.vector(linkinvx(X %*% parms[1:kx] + offsetx))
        theta <- exp(parms[kx + 1])
        d <- suppressWarnings(stats::dnbinom(Y,
            size = theta, mu = mu, log = TRUE))
        loglik <- sum(weights * d)
        if (!is.finite(loglik) || is.na(loglik))
            loglik <- -good.num.limit[2]
        loglik
    }
    nll_ZINB_ML <- function(parms) {
        mu <- as.vector(linkinvx(X %*% parms[1:kx] + offsetx))
        theta <- exp(parms[kx + 1])
        phi <- as.vector(linkinvz(Z %*% parms[(kx+1+1):(kx+kz+1)] + offsetz))
        loglik0 <- log(phi + exp(log(1 - phi) + suppressWarnings(stats::dnbinom(0,
            size = theta, mu = mu, log = TRUE))))
        loglik1 <- log(1 - phi) + suppressWarnings(stats::dnbinom(Y,
            size = theta, mu = mu, log = TRUE))
        loglik <- sum(weights[id0] * loglik0[id0]) + sum(weights[id1] *
            loglik1[id1])
        if (!is.finite(loglik) || is.na(loglik))
            loglik <- -good.num.limit[2]
        loglik
    }
    nll_ZINB_CL <- function(parms) {
        mu1 <- as.vector(linkinvx(X1 %*% parms[1:kx] + offsetx1))
        theta <- exp(parms[kx + 1])
        num <- suppressWarnings(stats::dnbinom(Y1,
            size = theta, mu = mu1, log = TRUE))
        den <- log(1 - exp(suppressWarnings(stats::dnbinom(0,
            size = theta, mu = mu1, log = TRUE))))
        loglik <- sum(weights1 * (num - den))
        if (!is.finite(loglik) || is.na(loglik))
            loglik <- -good.num.limit[2]
        loglik
    }
    logd0_ZINB <- function(parms) {
        mu <- as.vector(linkinvx(X %*% parms[1:kx] + offsetx))
        theta <- exp(parms[kx + 1])
        suppressWarnings(stats::dnbinom(0, size = theta, mu = mu, log = TRUE))
    }
    nll_ZINB_PL <- nll_ZIP_PL

    good.num.limit <- c(.Machine$double.xmin, .Machine$double.xmax)^(1/3)

    kx <- ncol(X)
    kz <- ncol(Z)
    np <- kx + kz
    n <- length(Y)
    if (distr == "negbin")
        np <- np + 1
    if (missing(offsetx))
        offsetx <- rep(0, n)
    if (missing(offsetz))
        offsetz <- rep(0, n)
    if (missing(weights))
        weights <- rep(1, n)
    linkinvx <- stats::poisson("log")$linkinv
    linkinvz <- stats::binomial(link)$linkinv
    id1 <- Y > 0
    id0 <- !id1
    W <- ifelse(id1, 1L, 0L)
    Y1 <- Y[id1]
    X1 <- X[id1,,drop=FALSE]
    Z1 <- Z[id1,,drop=FALSE]
    offsetx1 <- offsetx[id1]
    offsetz1 <- offsetz[id1]
    weights1 <- weights[id1]
    if (is.null(inits))
        inits <- rep(0, np)
    control$fnscale <- -1 # maximize

    ## CL conditional likelihood step
    nll_CL <- switch(distr,
        "poisson" = nll_ZIP_CL,
        "negbin" = nll_ZINB_CL)
    res_CL <- suppressWarnings(stats::optim(inits[1:(np-kz)], nll_CL,
        method=method, 
        hessian=FALSE, 
        control=control, ...))

    if (!zi) {
        nll_ML <- switch(distr,
            "poisson" = nll_P_ML,
            "negbin" = nll_NB_ML)
        res_CL$value <- nll_ML(res_CL$par)
        if (hessian) {
            res_CL$hessian <- stats::optimHess(res_CL$par, nll_ML)
        }
        return(res_CL)
    }

    ## PL pseudo likelihood step
    ## logd0 (log density for 0 obs) is plug-in
    ## logd0 needs to incorporate offsets but not weights
    logd0_fun <- switch(distr,
        "poisson" = logd0_ZIP,
        "negbin" = logd0_ZINB)
    logd0 <- logd0_fun(res_CL$par)

    nll_PL <- switch(distr,
        "poisson" = nll_ZIP_PL,
        "negbin" = nll_ZINB_PL)
    res_PL <- suppressWarnings(stats::optim(inits[(np-kz+1):np], nll_PL,
        logd0=logd0,
        method=method, 
        hessian=FALSE, 
        control=control, ...))

    ## CL+PL
    nll_ML <- switch(distr,
        "poisson" = nll_ZIP_ML,
        "negbin" = nll_ZINB_ML)
    res_CLPL <- list(par = c(res_CL$par, res_PL$par))
    res_CLPL$value <- nll_ML(res_CLPL$par)
    res_CLPL$convergence <- max(res_CL$convergence, res_PL$convergence)
    if (hessian) {
        res_CLPL$hessian <- stats::optimHess(res_CLPL$par, nll_ML)
    }
    if (distr == "negbin") {
        u <- c(1:kx, (kx+1+1):(kx+kz+1), kx+1)
        res_CLPL$par <- res_CLPL$par[u]
        res_CLPL$hessian <- res_CLPL$hessian[u,u]
    }

    res_CLPL
}
