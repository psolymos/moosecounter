devtools::load_all()

m <- sim_rsf() |>
    sim_strat(threshold = 0, p_misclass = 0.1) |>
    sim_expected(-1, 0.5, -1, 0.25) |>
    sim_counts(theta.nb = 0.1, "zeroinfl")
table(m$MOOSE_TOTA)
summary(m)

library(ggplot2)
m |>
    ggplot(aes(x = x, y = y, z = mu)) +
    geom_contour_filled() +
    geom_point(aes(x = x, y = y, col = MOOSE_TOTA), m[m$MOOSE_TOTA > 0, ]) +
    theme_light()

mc_options(B = 100)
switch_response("total")

surv_id <- sample(nrow(m), 100)
m$srv <- FALSE
m$srv[surv_id] <- TRUE
m$AREA_KM <- 16
# m$MOOSE_TOTA[!m$srv] <- 0

a <- c(
    "P" = "P",
    "NB" = "NB",
    # "HP" = "HP",
    # "HNB" = "HNB",
    "ZIP" = "ZIP",
    "ZINB" = "ZINB"
)

ML <- lapply(a, function(i) {
    mc_fit_total(m, vars = "z", zi_vars = "z", dist = i)
})
PIL <- list()
for (i in a) {
    PIL[[i]] <- mc_predict_total(
        model_id = i,
        ml = ML,
        x = m,
        do_boot = TRUE,
        do_avg = FALSE
    )
}
PIest <- lapply(PIL, pred_density_moose_PI)

list(
    total = sum(m$MOOSE_TOTA),
    estimates = sapply(PIest, function(z) z["Total_Moose", ]),
    gassaway = mc_gassaway(
        y1 = m$MOOSE_TOTA[m$srv & m$stratum == 0],
        y2 = m$MOOSE_TOTA[m$srv & m$stratum == 1],
        N1 = sum(1 - m$stratum),
        N2 = sum(m$stratum)
    )
)

library(intrval)
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

sapply(PIL, get_stats, true_vals = m$MOOSE_TOTA)
# use PIL[[1]]$data$Rank to pick where to go next
