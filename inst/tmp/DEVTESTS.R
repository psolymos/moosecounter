x <- read.csv("inst/extdata/MayoMMU_QuerriedData.csv")
switch_response("total")
x <- mc_update_total(x)

x$test <- as.factor(x$ELC_Subalpine < 0.5)

library(patchwork)
## continuous
i <- "SubShrub250_Fire8212DEM815"  # Continuous
i <- "test"                        # Categorical

p1 <- .plot_univariate(i, x, dist="ZINB", type = "density", interactive = TRUE)
p2 <- .plot_univariate(i, x, dist="ZINB", type = "map", interactive = TRUE)
p3 <- .plot_univariate(i, x, dist="ZINB", type = "fit", interactive = TRUE)

mc_ggiraph(p1 + p2 + p3, width = 15, height = 4)
