# moosecounter

> Adaptive Moose Surveys

Install:

```R
install.packages(c('dplyr', 'DT', 'ggiraph','ggplot2', 'kableExtra',
    'magrittr', 'mapproj', 'markdown', 'MASS', 'Matrix', 'openxlsx',
    'partykit', 'pbapply', 'pscl', 'purrr', 'remotes', 'shiny', 'shinyBS',
    'shinydashboard'))
remotes::install_github('psolymos/moosecounter')
```

Example:

```R
library(moosecounter)

## modify options as needed
mc_options(B=20)

## load/read data set
x <- read.csv(
    system.file("extdata/MayoMMU_QuerriedData.csv", 
        package="moosecounter"))

## set response in options
#switch_response("cows")
switch_response("total")

## define surveyed units and take a subset as needed
## this also sets total cows value
x <- mc_update_total(x)

## univariate exploration
mc_plot_univariate("Subalp_Shrub_250buf", x, "ZINB")

## multivariate exploration
vars <- c("ELC_Subalpine", "Fire1982_2012", "Fire8212_DEM815",
    "NALC_Needle", "NALC_Shrub", "Subalp_Shrub_250buf",
    "ELCSub_Fire8212DEM815", "SubShrub250_Fire8212DEM815")

mc_plot_multivariate(vars, x)

## build model list
ML <- list()
ML[["Model 0"]] <- mc_fit_total(x, dist="ZINB", weighted=TRUE)
ML[["Model 1"]] <- mc_fit_total(x, vars[1:2], dist="ZINB", weighted=TRUE)
ML[["Model 2"]] <- mc_fit_total(x, vars[2:3], dist="ZIP", weighted=TRUE)
ML[["Model 3"]] <- mc_fit_total(x, vars[3:4], dist="ZINB", weighted=TRUE)

mc_models_total(ML, x)
mc_plot_residuals("Model 3", ML, x)

## PI calculation
PI <- mc_predict_total(
    model_id=c("Model 1", "Model 3"),
    ml=ML,
    x=x,
    do_boot=TRUE, do_avg=TRUE)

mc_get_pred(PI)
pred_density_moose_PI(PI)
mc_plot_predpi(PI)
mc_plot_pidistr(PI)
mc_plot_pidistr(PI, id=2)

## Composition

# checks comp data (sum of classes should equal total)
mc_check_comp(x)

# plot univariate comp model
mc_plot_comp('Fire8212_DEM815', x)

CML <- list()
CML[['FireDEMSub']] <- mc_fit_comp(~ Fire8212_DEM815, x)

mc_models_comp(CML)

CPI <- mc_predict_comp(
    total_model_id="Model 3",
    comp_model_id='FireDEMSub',
    model_list_total=ML,
    model_list_comp=CML,
    x=x,
    do_avg=FALSE)
# print results
pred_density_moose_CPI(CPI)


## Shiny app
run_app()
```
