# Moose Composition Workflow

Fit composition model for Moose using a multinomial model to capture how
predictors affect composition data; calculate prediction intervals based
on composition model; and extract useful summaries.

## Usage

``` r
mc_check_comp(x)

mc_fit_comp(x, vars = NULL)

mc_models_comp(model_list_comp, coefs = TRUE)

mc_predict_comp(
  total_model_id,
  comp_model_id,
  model_list_total,
  model_list_comp,
  x,
  do_avg = FALSE,
  fix_mean = FALSE,
  PI = NULL
)

subset_CPI_data(CPI, ss)

pred_density_moose_CPI(CPI, ...)
```

## Arguments

- x:

  A Moose data frame object.

- vars:

  Column names of \`x\` to be used as predictors for the composition
  model.

- model_list_comp:

  Named list of total composition models.

- coefs:

  logical, return coefficient table too.

- total_model_id:

  Model ID or model IDs for total moose model (can be multiple model IDs
  from \`names(model_list_total)\`).

- comp_model_id:

  Model ID or model IDs for composition model (single model ID from
  \`names(model_list_comp)\`).

- model_list_total:

  Named list of total moose models.

- do_avg:

  Logical, to do model averaging or not.

- fix_mean:

  logical, use the fixed (rounded) mean as the Multinomial size instead
  of the bootstrap PI counts.

- PI:

  Total Moose PI object.

- CPI:

  Composition PI object.

- ss:

  A subset of rows (logical or numeric vector).

- ...:

  Other arguments passed to underlying functions.

## Examples

``` r
mc_options(B=10)
x <- read.csv(
    system.file("extdata/MayoMMU_QuerriedData.csv",
        package="moosecounter"))
## Prepare Moose data frame object
x <- mc_update_total(x)

## Total moose model list
vars <- c("ELC_Subalpine", "Fire1982_2012", "Fire8212_DEM815",
    "NALC_Needle", "NALC_Shrub", "Subalp_Shrub_250buf",
    "ELCSub_Fire8212DEM815", "SubShrub250_Fire8212DEM815")
ML <- list()
ML[["Model 0"]] <- mc_fit_total(x, dist="ZINB")
ML[["Model 1"]] <- mc_fit_total(x, vars[1:2], dist="ZINB")
ML[["Model 2"]] <- mc_fit_total(x, vars[2:3], dist="ZIP")
ML[["Model 3"]] <- mc_fit_total(x, vars[3:4], dist="ZINB")

## Composition odel list
CML <- list()
CML[['FireDEMSub']] <- mc_fit_comp(x, "Fire8212_DEM815")

## Stats from the models
mc_models_comp(CML)

## Calculate PI
CPI <- mc_predict_comp(
    total_model_id="Model 3",
    comp_model_id='FireDEMSub',
    model_list_total=ML,
    model_list_comp=CML,
    x=x,
    do_avg=FALSE)

## Predict density
pred_density_moose_CPI(CPI)
```
