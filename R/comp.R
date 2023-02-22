#' Moose Composition Workflow
#'
#' Fit composition model for Moose using a multinomial model
#' to capture how predictors affect composition data;
#' calculate prediction intervals based on composition model;
#' and extract useful summaries.
#'
#' @param total_model_id Model ID or model IDs for total moose
#'   model (can be multiple model IDs from `names(model_list_total)`).
#' @param comp_model_id Model ID or model IDs for composition model
#'   (single model ID from `names(model_list_comp)`).
#' @param model_list_total Named list of total moose models.
#' @param model_list_comp Named list of total composition models.
#' @param x A Moose data frame object.
#' @param do_avg Logical, to do model averaging or not.
#' @param vars Column names of `x` to be used as predictors
#'   for the composition model.
#' @param ss A subset of rows (logical or numeric vector).
#' @param CPI Composition PI object.
#' @param PI Total Moose PI object.
#' @param coefs logical, return coefficient table too.
#' @param fix_mean logical, use the fixed (rounded) mean as the Multinomial size
#'   instead of the bootstrap PI counts.
#' @param ... Other arguments passed to underlying functions.
#'
#' @examples
#' mc_options(B=10)
#' x <- read.csv(
#'     system.file("extdata/MayoMMU_QuerriedData.csv",
#'         package="moosecounter"))
#' ## Prepare Moose data frame object
#' x <- mc_update_total(x)
#'
#' ## Total moose model list
#' vars <- c("ELC_Subalpine", "Fire1982_2012", "Fire8212_DEM815",
#'     "NALC_Needle", "NALC_Shrub", "Subalp_Shrub_250buf",
#'     "ELCSub_Fire8212DEM815", "SubShrub250_Fire8212DEM815")
#' ML <- list()
#' ML[["Model 0"]] <- mc_fit_total(x, dist="ZINB", weighted=TRUE)
#' ML[["Model 1"]] <- mc_fit_total(x, vars[1:2], dist="ZINB", weighted=TRUE)
#' ML[["Model 2"]] <- mc_fit_total(x, vars[2:3], dist="ZIP", weighted=TRUE)
#' ML[["Model 3"]] <- mc_fit_total(x, vars[3:4], dist="ZINB", weighted=TRUE)
#'
#' ## Composition odel list
#' CML <- list()
#' CML[['FireDEMSub']] <- mc_fit_comp(x, "Fire8212_DEM815")
#'
#' ## Stats from the models
#' mc_models_comp(CML)
#'
#' ## Calculate PI
#' CPI <- mc_predict_comp(
#'     total_model_id="Model 3",
#'     comp_model_id='FireDEMSub',
#'     model_list_total=ML,
#'     model_list_comp=CML,
#'     x=x,
#'     do_avg=FALSE)
#'
#' ## Predict density
#' pred_density_moose_CPI(CPI)
#'
#' @keywords models regression
#' @name comp
NULL

#' @rdname comp
#' @export
## used to be called checkCompData
## x: MooseData
mc_check_comp <- function(x) {
  opts <- getOption("moose_options")
  x_srv <- x[x$srv,]
  if (opts$response == "total") {
      x_srv$TOT_CALVES <- x_srv$COW_1C + 2*x_srv$COW_2C + x_srv$LONE_CALF
      x_srv$ADULT_COW <- x_srv$COW_1C + x_srv$COW_2C + x_srv$LONE_COW
      x_srv$BULL_TOTA <- x_srv$BULL_LARGE + x_srv$BULL_SMALL
      x_srv$BULL_PER_COW <- x_srv$BULL_TOTA / x_srv$COW_TOTA
      check <- (x_srv$MOOSE_TOTA - x_srv$UNKNOWN_AG) - (x_srv$BULL_LARGE +
        x_srv$BULL_SMALL + x_srv$ADULT_COW + x_srv$TOT_CALVES)
      problem <- sum(abs(check) > 0.001)
      if (problem > 0)
        stop(paste("Composition data do not equal", opts$Ntot, "for", problem, "cases"))
  } else {
      x_srv$TOT_CALVES <- x_srv$COW_1C + 2*x_srv$COW_2C + x_srv$LONE_CALF
      x_srv$ADULT_COW <- x_srv$COW_1C + x_srv$COW_2C + x_srv$LONE_COW
  }
    invisible(NULL)
}


#' @rdname comp
#' @export
## fit comp model for total
## used to be called fitCompModel
## x: MooseData
mc_fit_comp <- function(x, vars=NULL) {
    opts <- getOption("moose_options")
    if (is.null(vars)) {
        CMP <- "1"
    } else {
        vars <- vars[!(vars %in% c(opts$Ntot, opts$composition))]
        CMP <- paste(vars, collapse=" + ")
    }
    Form <- stats::as.formula(paste("~", CMP))
    mc_fit_comp_formula(Form, x)
}

mc_fit_comp_formula <- function(formula, x) {
  opts <- getOption("moose_options")
  keep <- x$UNKNOWN_AG == 0
  if (any(x$UNKNOWN_AG > 0))
      warning(paste0(sum(x$UNKNOWN_AG > 0),
        " rows with unknown animals excluded"))
  x <- x[keep,,drop=FALSE]

  x_srv <- x[x$srv,]
  x_srv <- x_srv[x_srv[[opts$Ntot]] > 0,]
  #X <- model.matrix(formula, x_srv)
  x_srv$TOT_CALVES <- x_srv$COW_1C + 2*x_srv$COW_2C + x_srv$LONE_CALF
  #      ADULT_COW <- x_srv$COW_1C + x_srv$COW_2C + x_srv$LONE_COW
  if (opts$response == "total") {
      ymat <- cbind(
        BULL_LARGE=x_srv$BULL_LARGE,
        BULL_SMALL=x_srv$BULL_SMALL,
        COW_1C=x_srv$COW_1C,
        COW_2C=x_srv$COW_2C,
        LONE_COW=x_srv$LONE_COW,
        TOT_CALVES=x_srv$TOT_CALVES)
  } else {
      ymat <- cbind(
        COW_1C=x_srv$COW_1C,
        COW_2C=x_srv$COW_2C,
        LONE_COW=x_srv$LONE_COW,
        TOT_CALVES=x_srv$TOT_CALVES)
      ymat[rowSums(ymat)==0,] <- NA
  }
  formula <- stats::as.formula(paste("ymat", paste(as.character(formula), collapse=" ")))
  m <- VGAM::vglm(
    formula=formula,
    data=x_srv,
    family=VGAM::multinomial,
    na.action=stats::na.omit)
  Nam <- names(m@coefficients)
  for (i in seq_len(ncol(ymat)-1L)) {
    j <- grepl(paste0(":", i), Nam)
    names(m@coefficients)[j] <- gsub(paste0(":", i),
      paste0(":", colnames(ymat)[i]), names(m@coefficients)[j])
  }
  m@call$formula <- formula
  m
}

#checkCompModelList <- function() {
#    if (!exists("CompModelList"))
#        assign("CompModelList", list(), envir=.GlobalEnv)
#    invisible(NULL)
#}

# Organize comp coefs
get_coefs2 <- function(CML) {
    l <- lapply(CML, stats::coef)
    cfn <- unique(unname(unlist(lapply(l, names))))
    M <- matrix(NA_real_, length(CML), length(cfn))
    dimnames(M) <- list(names(CML), cfn)
    for (i in seq_len(length(CML))) {
        M[i,] <- l[[i]][match(cfn, names(l[[i]]))]
    }
    colnames(M) <- gsub("(", "", gsub(")", "", colnames(M), fixed=TRUE), fixed=TRUE)
    M
}

#' @rdname comp
#' @export
## used to be updateCompModelTab
mc_models_comp <- function(model_list_comp, coefs=TRUE) {
  ic <- data.frame(
    AIC=sapply(model_list_comp, VGAM::AIC),
    BIC=sapply(model_list_comp, VGAM::BIC))
  ic$delta <- ic$AIC - min(ic$AIC)
  rel <- exp(-0.5*ic$delta)
  ic$weight <- rel / sum(rel)
  if (coefs) {
    cf <- get_coefs2(model_list_comp)
    ic <- data.frame(ic, cf)
  }
  ic[order(ic$delta),]
}

#' @rdname comp
#' @export
#function(Survey.data, Unsurvey.data, B,
#moose.model, model.B, MAXCELL,
#Mult.model, alpha, TotalMMU.pred, MMU_ID)
## used to be MooseCompSimMMU.PI1
mc_predict_comp <- function(total_model_id, comp_model_id,
    model_list_total, model_list_comp, x, do_avg=FALSE,
    fix_mean = FALSE, PI=NULL) {

  newPI <- is.null(PI)
  if (!newPI) {
    total_model_id <- PI$model_id
    model_list_total <- PI$model_list
    x <- PI$data
    do_avg <- PI$do_avg
    PImean <- round(rowMeans(PI$boot_full))
    PIboot <- PI$boot_full
  } else {
    if (fix_mean)
      stop("fix_mean implies a non NULL PI object is provided")
  }

  opts <- getOption("moose_options")
  #x <- MooseData
  ## if some survey area is defined, use dual prediction
  DUAL <- !is.null(opts$area_srv)
  if (DUAL) {
      x$area_srv <- x[[opts$area_srv]]
      #eval(parse(text=paste0("x$area_srv <- x$", opts$area_srv)))
  } else {
      x$area_srv <- TRUE
  }

  # keep <- x$UNKNOWN_AG == 0
  # if (any(x$UNKNOWN_AG > 0))
  #     warning(sum(x$UNKNOWN_AG > 0),
  #       " rows with unknown animals excluded")
  # x <- x[keep,,drop=FALSE]

  # if (!newPI) {
  #   PImean <- PImean[keep]
  #   PIboot <- PIboot[keep,,drop=FALSE]
  # }

  x$sort_id <- 1:nrow(x)
  srv <- x$srv
  Survey.data <- x[srv,]
  if (opts$response != "total") {
    Survey.data$BULL_LARGE[] <- 0
    Survey.data$BULL_SMALL[] <- 0
  }

  Survey.data$TOT_CALVES <- Survey.data$COW_1C +
    2*Survey.data$COW_2C + Survey.data$LONE_CALF
  Unsurvey.data <- x[!srv,]

  B <- opts$B
  mid <- character(B)
  alpha <- opts$alpha
  MAXCELL <- if (is.null(opts$MAXCELL))
      2*max(x[srv,opts$Ntot], na.rm=TRUE) else opts$MAXCELL
  if (MAXCELL < max(x[srv,opts$Ntot], na.rm=TRUE))
      stop("MAXCELL must not be smaller than max observed total abundance")

  if (!all(total_model_id %in% names(model_list_total)))
      stop(paste0(total_model_id, " model cannot be found"))
  #wt <- updateModelTab()
  wt <- mc_models_total(model_list_total, x)
  wts <- wt[total_model_id,,drop=FALSE]
  total_model_id0 <- total_model_id
  #total_fit <- ModelList[[total_model_id]]

  if (length(comp_model_id) > 1L)
    stop("comp_model_id must be of length 1")
  if (!(comp_model_id %in% names(model_list_comp)))
    stop(paste(comp_model_id, " model cannot be found"))
  Mult.model <- model_list_comp[[comp_model_id]]

## ??? Total.pred should be recalculated inside the loop ???
#  pr_uns <- predict(total_fit, newdata=x[subset_uns,,drop=FALSE], type=c("response"))
#  Total.pred <- sum(pr_uns) + sum(Survey.data$MOOSE_TOTA[subset_srv])

  #  Total.pred <- TotalMMU.pred
  #  Unsurvey.data <- subset(Unsurvey.data[Unsurvey.data$MMU == MMU_ID])
  ## sum observed + predicted

#  parms.start <- list(
#    count = total_fit$coef$count,
#    zero = total_fit$coef$zero,
#    theta = total_fit$theta)

  BUnsurvey.data <- Unsurvey.data
  NS <- nrow(BUnsurvey.data)

  tmp <- matrix(0, nrow(x), B)
  all_ratios_list <- list(
    Total.pred = tmp,
    Total_LB =  tmp,
    Total_Calves = tmp,
    Total_Cows = tmp,
    Total_SB = tmp,
    Total_Yrlings = tmp,
    Total_Yrling_Cows = tmp,
    Total_Mature_Cows = tmp,
    Total_1C = tmp,
    Total_2C = tmp,
    Total_Bulls = tmp)

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

  index <- sample(seq(1:nrow(Survey.data)), nrow(Survey.data), replace=TRUE)
  BSurvey.data <- Survey.data[index,]
  if (max(BSurvey.data$MOOSE_TOTA) != 0) { # loop 1

    ## model selection
    if (newPI) {
      if (do_avg) {
          total_model_id <- sample(rownames(wts), 1, prob=wts$weight)
      } else {
          total_model_id <- rownames(wts)[which.max(wts$weight)]
          if (length(total_model_id) > 1) # this should really never happen
              total_model_id <- sample(total_model_id, 1)
      }
      mid[b] <- total_model_id
      total_fit <- model_list_total[[total_model_id]]
      parms.start <- list(
          count = total_fit$coef$count,
          zero = total_fit$coef$zero,
          theta = total_fit$theta)
      w <- rep(1, nrow(BSurvey.data))
      model.Boot <- try(suppressWarnings(stats::update(total_fit,
        #data = BSurvey.data,
        x = BSurvey.data,
        control = pscl::zeroinfl.control(
          start = parms.start,
          method = opts$method))), silent = TRUE)
    } else {
      total_fit <- model_list_total[[PI$model_select_id[b]]]
      model.Boot <- total_fit
      parms.start <- list(
          count = total_fit$coef$count,
          zero = total_fit$coef$zero,
          theta = total_fit$theta)
    }
    if (!inherits(model.Boot, "try-error")) {
        attr(model.Boot, "parms.start") <- parms.start
        if (inherits(total_fit, "wzi") && newPI)
          model.Boot <- wzi(model.Boot, pass_data=TRUE)

    #   model.Boot <- model.B(BSurvey.data)
        if (newPI) {
          predict.BNS <- stats::predict(model.Boot, newdata=Unsurvey.data, type="response")
          predict.BNSout <- if (DUAL && inherits(total_fit, "wzi")) {
              stats::predict(model.Boot$unweighted_model,
                  newdata=Unsurvey.data, type="response")
          } else {
              predict.BNS
          }
        } else {
          predict.BNS <- predict.BNSout <- PI$boot_full[!srv,b]
        }

        if (max(predict.BNS, predict.BNSout) <= MAXCELL & model.Boot$optim$convergence == 0) { # loop 2
          if (newPI) {
            ## here 'count' refers to the abundance model (no ZI considered)
            Bm.NS <- stats::predict(model.Boot, newdata=Unsurvey.data, type="count")
            Btheta.nb <- model.Boot$theta
            #Bphi.zi <- 1 - plogis(coef(model.Boot)[length(coef(model.Boot))])
            Bphi.zi <- 1 - stats::predict(model.Boot, newdata=Unsurvey.data, type="zero")
            PUnsurvey.data <- rZINB(NS, mu.nb=Bm.NS,
              theta.nb=Btheta.nb, phi.zi=Bphi.zi)
            if (DUAL && inherits(total_fit, "wzi")) {
              Bm.NSout <- stats::predict(model.Boot$unweighted_model,
                  newdata = Unsurvey.data[!Unsurvey.data$area_srv,], type="count")
              Btheta.nbout <- model.Boot$unweighted_model$theta
              Bphi.ziout <- 1 - stats::predict(model.Boot$unweighted_model,
                  newdata = Unsurvey.data[!Unsurvey.data$area_srv,], type="zero")
              PUnsurvey.data[!Unsurvey.data$area_srv] <- rZINB(sum(!Unsurvey.data$area_srv),
                  mu.nb = Bm.NSout,
                  theta.nb=Btheta.nbout,
                  phi.zi=Bphi.ziout)
            }
          } else {
            if (fix_mean) {
              PUnsurvey.data <- PImean[!srv]
            } else {
              PUnsurvey.data <- PIboot[!srv,b]
            }
          }

          if (max(PUnsurvey.data) <= MAXCELL) { # loop 3
            BUnsurvey.data$MOOSE_TOTA <- PUnsurvey.data
            newdata1 <- BUnsurvey.data[BUnsurvey.data$MOOSE_TOTA != 0,]
            NS1 <- nrow(newdata1)
            if (NS1 > 0) { # loop 4

              pred.prob <- VGAM::predict(Mult.model, newdata=newdata1, type="response")
              K <- ncol(pred.prob)

              pred.numbers1 <- matrix(-100,NS1,K)

              for (i in 1:NS1){
        	       pred.numbers1[i,] <- stats::rmultinom(1, newdata1$MOOSE_TOTA[i], pred.prob[i,])
              }

              pred.numbers <- matrix(0,NS,K)
              pred.numbers[BUnsurvey.data$MOOSE_TOTA != 0,] <- pred.numbers1
              if (opts$response == "total") {
                  pred.numbers <- data.frame(
                    BULL_LARGE = pred.numbers[,1],
                    BULL_SMALL = pred.numbers[,2],
                    COW_1C = pred.numbers[,3],
                    COW_2C = pred.numbers[,4],
                    LONE_COW = pred.numbers[,5],
                    TOT_CALVES = pred.numbers[,6])
              } else {
                  pred.numbers <- data.frame(
                    BULL_LARGE = 0,
                    BULL_SMALL = 0,
                    COW_1C = pred.numbers[,1],
                    COW_2C = pred.numbers[,2],
                    LONE_COW = pred.numbers[,3],
                    TOT_CALVES = pred.numbers[,4])
              }

              ## handle UNKNOWN_AG for surveyed data
              UNKwhere <- which(Survey.data$MOOSE_TOTA > 0 & Survey.data$UNKNOWN_AG > 0)
              unk_df <- Survey.data[UNKwhere,,drop=FALSE]
              UNK1 <- nrow(unk_df)
              if (UNK1 > 0) {
                pred.prob.unk <- VGAM::predict(Mult.model, newdata=unk_df, type="response")
                K <- ncol(pred.prob.unk)
                pred.unk1 <- matrix(-100,UNK1,K)
                for (i in 1:UNK1){
                  pred.unk1[i,] <- stats::rmultinom(1, unk_df$UNKNOWN_AG[i], pred.prob.unk[i,])
                }
                if (opts$response == "total") {
                      Survey.data$BULL_LARGE[UNKwhere] <- Survey.data$BULL_LARGE[UNKwhere] + pred.unk1[,1]
                      Survey.data$BULL_SMALL[UNKwhere] <- Survey.data$BULL_SMALL[UNKwhere] + pred.unk1[,2]
                      Survey.data$COW_1C[UNKwhere] <- Survey.data$COW_1C[UNKwhere] + pred.unk1[,3]
                      Survey.data$COW_2C[UNKwhere] <- Survey.data$COW_2C[UNKwhere] + pred.unk1[,4]
                      Survey.data$LONE_COW[UNKwhere] <- Survey.data$LONE_COW[UNKwhere] + pred.unk1[,5]
                      Survey.data$TOT_CALVES[UNKwhere] <- Survey.data$TOT_CALVES[UNKwhere] + pred.unk1[,6]
                } else {
                      Survey.data$COW_1C[UNKwhere] <- Survey.data$COW_1C[UNKwhere] + pred.unk1[,1]
                      Survey.data$COW_2C[UNKwhere] <- Survey.data$COW_2C[UNKwhere] + pred.unk1[,2]
                      Survey.data$LONE_COW[UNKwhere] <- Survey.data$LONE_COW[UNKwhere] + pred.unk1[,3]
                      Survey.data$TOT_CALVES[UNKwhere] <- Survey.data$TOT_CALVES[UNKwhere] + pred.unk1[,4]
                }
              }

              all_ratios_list$Total.pred[,b] <- c(Survey.data$MOOSE_TOTA,
                  rowSums(pred.numbers))
              all_ratios_list$Total_LB[,b] <- c(Survey.data$BULL_LARGE,
                  pred.numbers$BULL_LARGE)
              all_ratios_list$Total_Calves[,b] <- c(Survey.data$TOT_CALVES,
                  pred.numbers$TOT_CALVES)
              all_ratios_list$Total_Cows[,b] <- c(
                  Survey.data$COW_1C + Survey.data$COW_2C + Survey.data$LONE_COW,
                  pred.numbers$COW_1C + pred.numbers$COW_2C + pred.numbers$LONE_COW)
              all_ratios_list$Total_SB[,b] <- c(Survey.data$BULL_SMALL,
                  pred.numbers$BULL_SMALL)
              # need this to avoid negative Total_Mature_Cows numbers
              all_ratios_list$Total_Yrling_Cows[,b] <- pmin(all_ratios_list$Total_SB[,b], all_ratios_list$Total_Cows[,b])
              # all_ratios_list$Total_Yrlings[,b] <- 2 * all_ratios_list$Total_SB[,b]
              all_ratios_list$Total_Yrlings[,b] <- all_ratios_list$Total_Yrling_Cows[,b] + all_ratios_list$Total_SB[,b]
              # all_ratios_list$Total_Mature_Cows[,b] <-
              #     all_ratios_list$Total_Cows[,b] - all_ratios_list$Total_SB[,b]
              all_ratios_list$Total_Mature_Cows[,b] <-
                  all_ratios_list$Total_Cows[,b] - all_ratios_list$Total_Yrling_Cows[,b]
              all_ratios_list$Total_1C[,b] <- c(Survey.data$COW_1C,
                  pred.numbers$COW_1C)
              all_ratios_list$Total_2C[,b] <- c(Survey.data$COW_2C,
                  pred.numbers$COW_2C)
              all_ratios_list$Total_Bulls[,b] <- c(Survey.data$BULL_LARGE + Survey.data$BULL_SMALL,
                  pred.numbers$BULL_LARGE + pred.numbers$BULL_SMALL)

            pbapply::setpb(pb, b)
              b <- b + 1
            } # loop 4
          } # loop 3
        } # loop 2
    } else { # if
      ISSUES[[length(ISSUES)+1]] <- as.character(model.Boot)
    } # if
  } # loop 1
  } # while

    o <- order(c(Survey.data$sort_id, Unsurvey.data$sort_id))
    for (i in 1:length(all_ratios_list))
        all_ratios_list[[i]] <- all_ratios_list[[i]][o,,drop=FALSE]
    sc <- mc_summarize_composition(all_ratios_list)
    out <- list(total_model_id=total_model_id0,
        comp_model_id=comp_model_id,
        do_avg=do_avg,
        fix_mean=fix_mean,
        model_list_total=model_list_total,
        model_list_comp=model_list_comp,
        total_model_select_id=mid,
        alpha=alpha,
        boot_full=all_ratios_list,
        issues=ISSUES,
        data=x,
        cells=sc$cells,
        total=sc$total)
    out
}

## OK--need to summarize the whole stuff as array at cell level (n x vars x B)
## write a subset function and calculate the totals based on that
## keep cell min restriction there
## remove subset option from CPI dialogue
## add CPI subset dialog (similar to PI subset)
# used to be summarize_composition
mc_summarize_composition <- function(all_ratios_list) {
    opts <- getOption("moose_options")
    alpha <- opts$alpha
    dc <- lapply(all_ratios_list, function(z)
        t(apply(z, 1, function(zz) c(Mean=mean(zz), stats::quantile(zz, c(0.5, alpha/2,(1-alpha/2)))))))
    dcell <- do.call(cbind, dc)
    colnames(dcell) <- paste(rep(names(dc), each=ncol(dc[[1]])), colnames(dc[[1]]), sep="_")
    d <- sapply(all_ratios_list, colSums)
    d <- data.frame(d)
  ## Now we will compute the required ratios.
  ## They will be returned when the function is run.
    ## Number of calves per 100 cows
    d$CC.ratio <- (d$Total_Calves/d$Total_Mature_Cows)*100
    d$YC.ratio <- (d$Total_Yrlings/d$Total_Mature_Cows)*100
    ## Number of large bulls per 100 cows
    d$BC.ratio <- (d$Total_LB/d$Total_Mature_Cows)*100
    d$PC_BULL_LARGE <- d$Total_LB/d$Total.pred
    d$PC_COWS_MATURE <- d$Total_Mature_Cows/d$Total.pred
    d$PC_Yrlings <- d$Total_Yrlings/d$Total.pred
    d$PC_Calves <- d$Total_Calves/d$Total.pred
    d$Yrling_Recruitment <- d$Total_Yrlings/(d$Total.pred - d$Total_Calves)
    d$Twining_rate <- d$Total_2C/(d$Total_1C + d$Total_2C)
    ## Number of all large bulls per 100 cows
    d$Bulls_per_Cow <- (d$Total_Bulls/d$Total_Cows)*100
    d$Calves_per_Cows <- (d$Total_Calves / d$Total_Cows)*100
    dtot <- t(apply(d, 2, function(z) c(Mean=mean(z), stats::quantile(z, c(0.5, alpha/2, (1-alpha/2))))))
    list(total=dtot, cells=dcell, raw=d)
}


#' @rdname comp
#' @export
subset_CPI_data <- function(CPI, ss) {
    if (missing(ss))
        ss <- rep(TRUE, nrow(CPI$data))
    opts <- getOption("moose_options")
    CPIout <- CPI
    for (i in 1:length(CPI$boot_full))
        CPIout$boot_full[[i]] <- CPI$boot_full[[i]][ss,,drop=FALSE]
    CPIout$total <- mc_summarize_composition(CPIout$boot_full)$total
    CPIout$cells <- CPI$cells[ss,,drop=FALSE]
    CPIout$data <- CPI$data[ss,,drop=FALSE]

    if (nrow(CPIout$data) < opts$MINCELL)
        stop(paste0("Composition PIs cannot be provided for <",
            opts$MINCELL, " cells"))
    CPIout
}

#' @rdname comp
#' @export
pred_density_moose_CPI <- function(CPI, ...){
    out <- round(CPI$total, 2)
    cat("Composition PI summary:\n\n")
    print(out, ...)
    if (length(CPI$issues) > 0L) {
        cat("\nNote:", length(CPI$issues),
            "issues were found during CPI calculations.\n\n")
    }
    invisible(out)
}

# import from VGAM does not work for some reason
vlm <- VGAM::vlm

