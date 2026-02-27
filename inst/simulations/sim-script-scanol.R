# Rscript --vanilla inst/simulations/sim-script-scanol.R --notify --model P

t0 <- proc.time()
finished <- FALSE
library(rconfig)
library(intrval)
devtools::load_all()

CONFIG <- rconfig::rconfig()
str(CONFIG)

DIR <- value(CONFIG$dir, "_tmp/simuls")
MODEL <- value(CONFIG$model, "P")
FIXED_SAMPLE_SIZE <- value(CONFIG$sample, 100)
B <- value(CONFIG$B, 500) # should be 500-1000, bootstrap iters
N <- value(CONFIG$N, 200) # >200 for sure, number of sim runs
SEED <- value(CONFIG$seed, 0)
MAXCELL <- value(CONFIG$maxcell, 100)
notify <- rconfig::value(CONFIG$notify, FALSE)
.Last <- function() {
    if (notify) {
        topic <- "a8m_bsims_alerts"
        err <- if (finished) "" else paste0("\n", geterrmessage())
        details <- paste0(
            "model=",
            MODEL,
            ", N=",
            as.character(N),
            ", B=",
            as.character(B),
            collapse = ""
        )
        tm <- as.numeric(proc.time()[3] - t0[3]) / 60
        tm <- if (tm < 60) {
            paste0(round(tm, 1), " mins")
        } else {
            if (tm < 24 * 60) {
                paste0(round(tm / 60, 1), " hrs")
            } else {
                paste0(
                    tm %/% (60 * 24),
                    " days & ",
                    round(tm %% (60 * 24) / 60),
                    " hrs"
                )
            }
        }
        msg <- paste(
            "Finished job in",
            tm,
            "@",
            format(.POSIXct(Sys.time(), "America/Edmonton")),
            "\nJob details:",
            details,
            err
        )
        if (finished) {
            icon <- "\"X-Tags: partying_face\""
            pri <- "\"X-Priority: 3\""
        } else {
            icon <- "\"X-Tags: triangular_flag_on_post\""
            pri <- "\"X-Priority: 4\""
        }
        system2(
            "curl",
            c(
                "-H",
                icon,
                "-H",
                pri,
                "-d",
                sprintf("\"%s\"", msg),
                sprintf("ntfy.sh/%s", topic)
            )
        )
    }
    if (finished) {
        message("DONE")
    } else {
        quit("no", 1L, FALSE)
    }
}
options(error = .Last)

m0 <- read.csv("inst/simulations/SouthCanol2022_forSimul.csv")

MOD_LIST <- c("P", "NB", "ZIP", "ZINB", "HP", "HNB")

pbapply::pboptions(type = "timer")
mc_options(B = B, MAXCELL = MAXCELL)
switch_response("total")
set.seed(SEED)

get_stats <- function(PI, true_vals) {
    is_srv <- PI$data$srv
    cell_q <- t(apply(
        PI$boot_full,
        1,
        quantile,
        c(0.025, 0.05, 0.95, 0.975)
    ))
    in_95 <- true_vals %[]% cell_q[, c(1, 4)]
    in_90 <- true_vals %[]% cell_q[, c(2, 3)]
    tot <- colSums(PI$boot_full)
    tot_mean <- mean(tot)
    tot_median <- median(tot)
    tot_mode <- moosecounter::find_mode(tot)
    tot_q <- quantile(tot, c(0.025, 0.05, 0.95, 0.975))
    c(
        true_total = sum(true_vals),
        mean = tot_mean,
        median = tot_median,
        mode = tot_mode,
        tot_q,
        coverage_90 = sum(in_90[!is_srv]) / sum(!is_srv),
        coverage_95 = sum(in_95[!is_srv]) / sum(!is_srv)
    )
}

TYPE <- switch(
    MODEL,
    "P" = "none",
    "NB" = "none",
    "HP" = "hurdle",
    "HNB" = "hurdle",
    "ZIP" = "zeroinfl",
    "ZINB" = "zeroinfl"
)

if (MODEL == "P") {
    COEF <- list(a = 0.1179619, b = 1.1577387, a1 = -999, b1 = -999)
    THETA_NB <- NULL
}
if (MODEL == "NB") {
    COEF <- list(a = -0.6508765, b = 1.8426342, a1 = -999, b1 = -999)
    THETA_NB <- 0.5326076
}
if (MODEL %in% c("ZIP")) {
    COEF <- list(a = 1.0257663, b = 0.7715055, a1 = -1.8267479, b1 = 2.5016767)
    THETA_NB <- NULL
}
if (MODEL %in% c("HP")) {
    COEF <- list(a = 1.0273936, b = 0.7705437, a1 = -1.8781851, b1 = 2.5435980)
    THETA_NB <- NULL
}
if (MODEL %in% c("ZINB")) {
    COEF <- list(a = 0.6500897, b = 0.9511952, a1 = -2.1796556, b1 = 3.7853855)
    THETA_NB <- 1.077793
}
if (MODEL %in% c("HNB")) {
    COEF <- list(a = 0.5979696, b = 1.0078578, a1 = -1.8781851, b1 = 2.5435980)
    THETA_NB <- 1.163009
}
# depends on MODEL
m1 <- sim_expected(
    m0,
    intercept = COEF$a,
    slope = COEF$b,
    intercept1 = COEF$a1,
    slope1 = COEF$b1
)
MT <- matrix(0, nrow(m0), N)

RES <- list()
SRV <- 0 * MT
LL <- matrix(0, N, length(MOD_LIST))
IC <- list()
EST <- list()
colnames(LL) <- MOD_LIST

k <- 1
while (k <= N) {
    message(MODEL, ": run ", k, " [", Sys.time(), "]")

    m <- sim_counts(m1, theta.nb = THETA_NB, type = TYPE)
    MT[, k] <- m$MOOSE_TOTA

    surv_id <- sample(nrow(m), FIXED_SAMPLE_SIZE)
    SRV[, k] <- 0
    SRV[surv_id, k] <- 1
    m$srv <- FALSE
    m$srv[surv_id] <- TRUE
    m$AREA_KM <- 16
    summary(m)

    ML <- list()
    for (i in MOD_LIST) {
        ML[[i]] <- try(mc_fit_total(m, vars = "z", zi_vars = "z", dist = i))
    }
    if (all(!sapply(ML, inherits, "try-error"))) {
        EST[[k]] <- lapply(ML, \(z) {
            c(
                coef(z),
                theta.nb = if (is.null(z$theta)) NA else unname(unlist(z$theta))
            )
        })
        PL <- list()
        FAILED <- 0
        OK <- FALSE
        while (!OK) {
            for (i in MOD_LIST) {
                if (is.null(PL[[i]]) || inherits(PL[[i]], "try-error")) {
                    message("  - PI: ", i)
                    PL[[i]] <- try(mc_predict_total(
                        model_id = i,
                        ml = ML,
                        x = m,
                        do_boot = TRUE,
                        do_avg = FALSE
                    ))
                }
            }
            if (all(!sapply(PL, inherits, "try-error"))) {
                RES[[k]] <- sapply(PL, get_stats, true_vals = m$MOOSE_TOTA)
                LL[k, ] <- sapply(ML, \(z) {
                    as.numeric(logLik(z))
                })
                IC[[k]] <- mc_models_total(ML, m)
                k <- k + 1
                OK <- TRUE
            } else {
                FAILED <- FAILED + 1
                if (FAILED > 12) {
                    break
                }
            }
        }
    }
}

fn <- sprintf(
    "%s/moose-sim_scanol_%s_N-%s_B-%s_%s.RData",
    DIR,
    MODEL,
    as.character(N),
    as.character(B),
    paste0(sample(c(0:9, letters), 6, replace = TRUE), collapse = "")
)
save(
    m0,
    MT,
    SRV,
    LL,
    IC,
    EST,
    RES,
    MODEL,
    COEF,
    THETA_NB,
    B,
    N,
    SEED,
    file = fn
)
message("Results saved: ", fn)

finished <- TRUE
quit("no", 0L, TRUE)
