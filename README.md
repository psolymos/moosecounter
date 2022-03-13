# moosecounter

> Adaptive Moose Surveys

The R package and Shiny app
implements count models to aid in estimating Moose populations
and composition. Analyses are based on surveyed cells,
residuals, point predictions, and prediction uncertainty
help analysts to adaptively survey sampling units.
As a result of adaptive sampling, models and estimates
become more accurate. This improvement enables
better management of moose populations.

_The development of the R package and the Shiny app was funded
by the Government of Yukon._

## Install

Install dependencies and the package from GitHub:

```R
install.packages(c('dplyr', 'DT', 'ggiraph','ggplot2', 'kableExtra',
    'magrittr', 'mapproj', 'markdown', 'MASS', 'Matrix', 'openxlsx',
    'partykit', 'pbapply', 'pscl', 'purrr', 'remotes', 'shiny', 'shinyBS',
    'shinydashboard'))
remotes::install_github('psolymos/moosecounter')
```

## Example

Load the package and set some options:

```R
library(moosecounter)

## modify options as needed
mc_options(B=20)
```

Load an example data from the Mayo:

```R
## load/read data set
x <- read.csv(
    system.file("extdata/MayoMMU_QuerriedData.csv", 
        package="moosecounter"))
```

Switch between modeling total vs. cows only:

```R
## set response in options
#switch_response("cows")
switch_response("total")
```

Ddefine surveyed units and take a subset as needed
(this also sets total cows value):

```R
x <- mc_update_total(x)
```

Start univariate and multivariate exploration to see what drives
the total population numbers:

```R
## univariate exploration
mc_plot_univariate("Subalp_Shrub_250buf", x, "ZINB")

## multivariate exploration
vars <- c("ELC_Subalpine", "Fire1982_2012", "Fire8212_DEM815",
    "NALC_Needle", "NALC_Shrub", "Subalp_Shrub_250buf",
    "ELCSub_Fire8212DEM815", "SubShrub250_Fire8212DEM815")

mc_plot_multivariate(vars, x)
```

Now it is time to build a list of models for the total Moose models:

```R
## build model list
ML <- list()
ML[["Model 0"]] <- mc_fit_total(x, dist="ZINB", weighted=TRUE)
ML[["Model 1"]] <- mc_fit_total(x, vars[1:2], dist="ZINB", weighted=TRUE)
ML[["Model 2"]] <- mc_fit_total(x, vars[2:3], dist="ZIP", weighted=TRUE)
ML[["Model 3"]] <- mc_fit_total(x, vars[3:4], dist="ZINB", weighted=TRUE)
```

Compare estimates and plot residuals:

```R
mc_models_total(ML, x)
mc_plot_residuals("Model 3", ML, x)
```

Calculate prediction intervals (PI) for total moose predictions:

```R
## PI calculation
PI <- mc_predict_total(
    model_id=c("Model 1", "Model 3"),
    ml=ML,
    x=x,
    do_boot=TRUE, do_avg=TRUE)
```

Get summaries and visualize the results for total Moose:

```R
mc_get_pred(PI)
pred_density_moose_PI(PI)
mc_plot_predpi(PI)
mc_plot_pidistr(PI)
mc_plot_pidistr(PI, id=2)
```

## Composition

Continuing with the Mayo data.
Checks comp data (sum of classes should equal total):

```R
mc_check_comp(x)
```

Explore relationships:

```R
# plot univariate comp model
mc_plot_comp("Fire8212_DEM815", x)
```

Build a model list for composition:

```R
CML <- list()
CML[['FireDEMSub']] <- mc_fit_comp(x, "Fire8212_DEM815")

mc_models_comp(CML)
```

Calculate prediction intervals for composition:

```R
CPI <- mc_predict_comp(
    total_model_id="Model 3",
    comp_model_id='FireDEMSub',
    model_list_total=ML,
    model_list_comp=CML,
    x=x,
    do_avg=FALSE)
# print results
pred_density_moose_CPI(CPI)
```

## Shiny app

Instead of using the command line, you can conduct the analyses,
including total Moose and composition, using an interactive
Shiny application:

```R
run_app()
```
