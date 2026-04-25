# Exploration

Functions to explore relationships between total Moose or composition as
response vs. environmental predictor variables.

## Usage

``` r
mc_plot_univariate(
  i,
  x,
  dist = "ZINB",
  base = TRUE,
  type = c("density", "map", "fit"),
  interactive = FALSE
)

mc_plot_multivariate(vars, x, alpha = NULL)

mc_plot_comp(i, x)
```

## Arguments

- i:

  Column name from \`x\` to be used as a predictor.

- x:

  Data frame with Moose data.

- dist:

  Count distribution (\`P\`, \`NB\`, \`ZIP\`, or \`ZINB\`).

- base:

  Logical, draw base graphics or ggplot2.

- type:

  Character, type of plot to be drawn (\`"density"\`, \`"map"\`,
  \`"fit"\`). Base plot can draw all 3, ggplot2 can only draw one at a
  time.

- interactive:

  Logical, draw interactive plot (not available for base plots).

- vars:

  A vector of column names from \`x\` to be used as a predictor.

- alpha:

  Alpha level defining \`mincriterion = 1 - alpha\` for
  \`partykit::ctree()\`.

## Details

\`mc_plot_univariate\` implements visual univariate (single predictor)
exploration for the total Moose count models.

\`mc_plot_multivariate\` implements visual multivariate (multiple
predictors) exploration based on regression trees (recursive
partitioning in a conditional inference framework) for total Moose
counts.

\`mc_plot_comp\` implements visual univariate (single predictor)
exploration for the multinomial composition models.

## Examples

``` r
## Prepare Moose data from Mayo
x <- read.csv(
    system.file("extdata/MayoMMU_QuerriedData.csv",
        package="moosecounter"))
switch_response("total")
x <- mc_update_total(x)

## Univariate exploration for total Moose
mc_plot_univariate("Subalp_Shrub_250buf", x, "ZINB")

## Multivariate exploration for total Moose
vars <- c("ELC_Subalpine", "Fire1982_2012", "Fire8212_DEM815",
    "NALC_Needle", "NALC_Shrub", "Subalp_Shrub_250buf",
    "ELCSub_Fire8212DEM815", "SubShrub250_Fire8212DEM815")
mc_plot_multivariate(vars, x)

## Univariate exploration for composition
mc_plot_comp("Fire8212_DEM815", x)
```
