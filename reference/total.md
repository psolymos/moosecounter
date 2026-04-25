# Total Moose Workflow

\`switch_response\` switches between total Moose vs. cows only. This
sets the column name for total Moose estimation. \`mc_update_total\`
Updates/prepares the Moose data set for downstream analyses (i.e.
calculates some derived variables, sets a surveyed/unsurveyed indicator,
and optionally takes a subset). \`mc_fit_total\` fit total Moose
abundance models. \`mc_models_total\` prints out estimates from the
models.

## Usage

``` r
switch_response(type = "total")

mc_update_total(x, srv = NULL, ss = NULL)

mc_fit_total(
  x,
  vars = NULL,
  zi_vars = NULL,
  dist = "ZINB",
  weighted = FALSE,
  robust = FALSE,
  intercept = c("both", "count", "zero", "none"),
  xv = FALSE,
  ...
)

mc_models_total(ml, x, coefs = TRUE)

mc_predict_total(model_id, ml, x, do_boot = TRUE, do_avg = FALSE)

mc_get_pred(PI, ss = NULL)

pred_density_moose_PI(PI)

mc_plot_residuals(model_id, ml, x)

mc_plot_predpi(PI)

mc_plot_pidistr(PI, id = NULL, plot = TRUE, breaks = "Sturges")

mc_plot_predfit(i, PI, ss = NULL, interactive = FALSE)
```

## Arguments

- type:

  The type of the response, can be \`"total"\` or \`"cows"\` for
  \`switch_response\`.

- x:

  A data frame with Moose data, or a data frame from
  \`mc_update_total()\`.

- srv:

  Logical vector, rows of \`x\` that are surveyed, falls back to global
  options when \`NULL\`.

- ss:

  Logical vector to subset \`x\`, default is to take no subset.

- vars:

  column names of \`x\` to be used as predictors for the count model.

- zi_vars:

  optional, column names of \`x\` to be used as predictors for the zero
  model.

- dist:

  Count distribution (\`P\`, \`NB\`, \`ZIP\`, \`ZINB\`).

- weighted:

  Logical, to use weighting to moderate influential observations.

- robust:

  Logical, use robust regression approach.

- intercept:

  Which intercepts to keep. Dropped intercepts lead to regression
  through the origin (at the linear predictor scale).

- xv:

  Logical, should leave-one-out error be calculated.

- ...:

  Other arguments passed to \`zeroinfl2()\`.

- ml:

  Named list of models.

- coefs:

  logical, return coefficient table too.

- model_id:

  model ID or model IDs (can be multiple from \`names(ml)\`).

- do_boot:

  Logical, to do bootstrap or not.

- do_avg:

  Logical, to do model averaging or not.

- PI:

  PI object returned by \`mc_predict_total()\`

- id:

  Cell ID.

- plot:

  Logical, to plot or just give summary.

- breaks:

  Breaks argument passed to \`graphics::hist()\`.

- i:

  Column (variable) name or index.

- interactive:

  Logical, draw interactive plot.

## Examples

``` r

mc_options(B=10)

x <- read.csv(
    system.file("extdata/MayoMMU_QuerriedData.csv",
        package="moosecounter"))

#switch_response("cows") # for cows only
switch_response("total")

x <- mc_update_total(x)

mc_plot_univariate("Subalp_Shrub_250buf", x, "ZINB")

vars <- c("ELC_Subalpine", "Fire1982_2012", "Fire8212_DEM815",
    "NALC_Needle", "NALC_Shrub", "Subalp_Shrub_250buf",
    "ELCSub_Fire8212DEM815", "SubShrub250_Fire8212DEM815")

mc_plot_multivariate(vars, x)

ML <- list()
ML[["Model 0"]] <- mc_fit_total(x, dist="ZINB")
ML[["Model 1"]] <- mc_fit_total(x, vars[1:2], dist="ZINB")
ML[["Model 2"]] <- mc_fit_total(x, vars[2:3], dist="ZIP")
ML[["Model 3"]] <- mc_fit_total(x, vars[3:4], dist="ZINB")

mc_models_total(ML, x)
mc_plot_residuals("Model 3", ML, x)

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
```
