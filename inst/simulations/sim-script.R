MODEL <- "ZINB"
GRID_SIZE <- 50
FIXED_SAMPLE_SIZE <- 100
THETA_NB <- 0.5
B <- 100
N <- 25

library(intrval)
devtools::load_all()

mc_options(B = B)
switch_response("total")

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
    tot_q <- quantile(tot, c(0.025, 0.05, 0.95, 0.975))
    c(
        true_total = sum(true_vals),
        mean = tot_mean,
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

m0 <- sim_rsf(dim_x = GRID_SIZE, dim_y = GRID_SIZE) |>
    sim_expected(-0.5, 1, -1.5, 0.5)

RES <- list()
for (k in seq_along(N)) {
    m <- sim_counts(m0, theta.nb = theta_nb, type = TYPE)

    table(m$MOOSE_TOTA)
    summary(m)

    surv_id <- sample(nrow(m), FIXED_SAMPLE_SIZE)
    m$srv <- FALSE
    m$srv[surv_id] <- TRUE
    m$AREA_KM <- 16
    summary(m)

    ML <- list(
        "P" = mc_fit_total(m, vars = "z", zi_vars = "z", dist = "P"),
        "NB" = mc_fit_total(m, vars = "z", zi_vars = "z", dist = "NB"),
        "ZIP" = mc_fit_total(m, vars = "z", zi_vars = "z", dist = "ZIP"),
        "ZINB" = mc_fit_total(m, vars = "z", zi_vars = "z", dist = "ZINB")
    )
    PIL <- list()
    for (i in a) {
        PIL[[i]] <- try(mc_predict_total(
            model_id = i,
            ml = ML,
            x = m,
            do_boot = TRUE,
            do_avg = FALSE
        ))
    }

    sapply(PIL, get_stats, true_vals = m$MOOSE_TOTA)
}
