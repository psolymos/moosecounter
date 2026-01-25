# Rscript --vanilla inst/simulations/sim-script.R --model ZIP

library(rconfig)
library(intrval)
devtools::load_all()

CONFIG <- rconfig::rconfig()
str(CONFIG)

DIR <- value(CONFIG$dir, "_tmp/simuls")
MODEL <- value(CONFIG$model, "P")
GRID_SIZE <- value(CONFIG$grid, 50)
FIXED_SAMPLE_SIZE <- value(CONFIG$sample, 250)
THETA_NB <- value(CONFIG$theta, 2)
B <- value(CONFIG$B, 100) # should be 500-1000
N <- value(CONFIG$N, 100) # >200 for sure
SEED <- value(CONFIG$seed, 0)

pbapply::pboptions(type = "none")
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
    tot_mode <- moosecounter::find_mode(x)
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
if (MODEL == "ZIP") {
    COEF <- list(a = 0, b = 1.2, a1 = -1, b1 = 0.25)
}
if (MODEL == "ZINB") {
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
for (i in seq_len(N)) {
    # Counts vary by run
    m1 <- sim_counts(m1, theta.nb = theta_nb, type = TYPE)
    MT[, i] <- m1$MOOSE_TOTA
}
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
    m <- data.frame(m0, MOOSE_TOTA = MT[, k])

    surv_id <- sample(nrow(m), FIXED_SAMPLE_SIZE)
    SRV[, k] <- 0
    SRV[surv_id, k] <- 1
    m$srv <- FALSE
    m$srv[surv_id] <- TRUE
    m$AREA_KM <- 16
    summary(m)

    ML <- list()
    for (i in c("P", "NB", "ZIP", "ZINB")) {
        ML[[i]] <- try(mc_fit_total(m, vars = "z", zi_vars = "z", dist = i))
    }
    if (all(!sapply(ML, inherits, "try-error"))) {
        PL <- list()
        for (i in names(ML)) {
            PL[[i]] <- try(mc_predict_total(
                model_id = i,
                ml = ML,
                x = m,
                do_boot = TRUE,
                do_avg = FALSE
            ))
        }
        if (all(!sapply(PL, inherits, "try-error"))) {
            RES[[k]] <- sapply(PL, get_stats, true_vals = m$MOOSE_TOTA)
            k <- k + 1
        } else {
            FAILED <- FAILED + 1
        }
    }
}

fn <- sprintf("%s/moose-sim_model-%s.RData", DIR, MODEL)
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

if (FALSE) {
    library(intrval)
    library(ggplot2)
    library(dplyr)

    DIR <- "_tmp/simuls"
    # MODEL <- "ZINB"
    dd <- NULL
    for (MODEL in c("P", "NB", "ZIP", "ZINB")) {
        fn <- sprintf("%s/moose-sim_model-%s.RData", DIR, MODEL)
        load(fn)

        d <- do.call(
            rbind,
            lapply(RES, \(x) {
                z <- data.frame(
                    true_model = MODEL,
                    model = colnames(x),
                    as.data.frame(t(x))
                )
            })
        )
        d$pi_95 <- as.numeric(d[, "true_total"] %[]% d[, c("X2.5.", "X97.5.")])
        d$pi_90 <- as.numeric(d[, "true_total"] %[]% d[, c("X5.", "X95.")])
        d$bias <- d$mean - d$true_total
        d$rel_bias <- (d$mean - d$true_total) / d$true_total

        dd <- rbind(dd, d)
    }

    dd |>
        group_by(true_model, model) |>
        summarize(pi_90 = mean(pi_90), pi_95 = mean(pi_95))

    dd |>
        ggplot(aes(x = true_model, y = coverage_95, col = model)) +
        geom_boxplot() +
        theme_light()
    dd |>
        ggplot(aes(x = true_model, y = coverage_90, col = model)) +
        geom_boxplot() +
        theme_light()

    dd |>
        ggplot(aes(x = true_model, y = true_total, col = model)) +
        geom_boxplot() +
        theme_light()

    dd |>
        ggplot(aes(x = true_model, y = bias, col = model)) +
        geom_boxplot() +
        theme_light()
    dd |>
        ggplot(aes(x = true_model, y = rel_bias, col = model)) +
        geom_boxplot() +
        theme_light()
}
