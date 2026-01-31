# Rscript --vanilla inst/simulations/sim-script.R --B 100 --model NB

library(rconfig)
library(intrval)
devtools::load_all()

CONFIG <- rconfig::rconfig()
str(CONFIG)

DIR <- value(CONFIG$dir, "_tmp/simuls")
MODEL <- value(CONFIG$model, "P")
GRID_SIZE <- value(CONFIG$grid, 50)
FIXED_SAMPLE_SIZE <- value(CONFIG$sample, 500)
THETA_NB <- value(CONFIG$theta, 10)
B <- value(CONFIG$B, 500) # should be 500-1000, bootstrap iters
N <- value(CONFIG$N, 100) # >200 for sure, number of sim runs
SEED <- value(CONFIG$seed, 0)

# MOD_LIST <- c("P", "NB", "ZIP", "ZINB")
MOD_LIST <- c("P", "NB", "ZIP", "ZINB", "HP", "HNB")

# pbapply::pboptions(type = "none")
mc_options(B = B)
switch_response("total")
set.seed(SEED)
m0 <- sim_rsf(dim_x = GRID_SIZE, dim_y = GRID_SIZE) # same for given seed

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
theta_nb <- if (MODEL %in% c("NB", "HNB", "ZINB")) {
    THETA_NB
} else {
    NULL
}

if (MODEL == "P") {
    COEF <- list(a = -0.5, b = 1, a1 = 0, b1 = 0)
}
if (MODEL == "NB") {
    COEF <- list(a = -0.8, b = 1, a1 = 0, b1 = 0)
}
if (MODEL %in% c("ZIP", "HP")) {
    COEF <- list(a = 0, b = 1.2, a1 = -1, b1 = 0.25)
}
if (MODEL %in% c("ZINB", "HNB")) {
    COEF <- list(a = -0.2, b = 1.2, a1 = -1, b1 = 0.25)
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
# for (i in seq_len(N)) {
#     # Counts vary by run
#     m1 <- sim_counts(m1, theta.nb = theta_nb, type = TYPE)
#     MT[, i] <- m1$MOOSE_TOTA
# }
# summary(colSums(MT))
# summary(colSums(MT > 0)) / nrow(MT)
# summary(apply(MT, 2, max))

RES <- list()
k <- 1
FAILED <- 0
SRV <- 0 * MT
while (k <= N) {
    message(MODEL, ": run ", k, " [", Sys.time(), "]")
    # m <- sim_counts(m0, theta.nb = theta_nb, type = TYPE)
    # table(m$MOOSE_TOTA)
    # summary(m)

    # m <- data.frame(m0, MOOSE_TOTA = MT[, k])
    m <- sim_counts(m1, theta.nb = theta_nb, type = TYPE)
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
        PL <- list()
        for (i in MOD_LIST) {
            message("  - PI: ", i)
            PL[[i]] <- try(mc_predict_total(
                model_id = i,
                ml = ML,
                x = m,
                do_boot = TRUE,
                do_avg = FALSE
            ))
        }
        # PL[["MM"]] <- try(mc_predict_total(
        #     model_id = names(ML),
        #     ml = ML,
        #     x = m,
        #     do_boot = TRUE,
        #     do_avg = FALSE
        # ))
        if (all(!sapply(PL, inherits, "try-error"))) {
            RES[[k]] <- sapply(PL, get_stats, true_vals = m$MOOSE_TOTA)
            k <- k + 1
        } else {
            FAILED <- FAILED + 1
        }
    }
}

fn <- sprintf(
    "%s/moose-sim_model-%s_n-%s_B-%s.RData",
    DIR,
    MODEL,
    as.character(FIXED_SAMPLE_SIZE),
    as.character(B)
)
save(
    m0,
    MT,
    SRV,
    RES,
    FAILED,
    MODEL,
    COEF,
    THETA_NB,
    GRID_SIZE,
    B,
    N,
    SEED,
    file = fn
)
quit("no")


library(ggplot2)
k <- 10^4
mu <- runif(k, 0, 10)
theta <- 10
mu2 <- (mu * rgamma(k, theta)) / theta
ggplot(data.frame(mu = mu, mu2 = mu2), aes(x = mu, y = mu2)) +
    geom_point() +
    geom_smooth()

lm(mu2 ~ mu)
