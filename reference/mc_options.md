# Set Moose Options

A function to get and set the package related options.

## Usage

``` r
mc_options(...)
```

## Arguments

- ...:

  options to set (see Details)

## Details

\* \`method\`: Optimization algorithm \* \`response\`: Response variable
\* \`MAXCELL\`: Max total abundance in cells \* \`MINCELL\`: Min
abundance for composition in cells \* \`B\`: Number of bootstrap runs \*
\`alpha\`: Type I error rate for PI \* \`wscale\`: Weight \*
\`sightability\`: Sightability \* \`seed\`: Random seed \* \`Ntot\`: set
to \`MOOSE_TOTA\` (internally changing to \`"COW_TOTA"\` when modeling
cows) \* \`srv_name\`: Filtering variable \* \`srv_value\`: Filtering
value \* \`area_srv\`: Column indicating survey areas \* \`Area\`: Area
\* \`xy\`: Long/Lat \* \`composition\`: Composition variables

## Examples

``` r
## original values
o <- mc_options()
str(o)
o$B

## set B to new value
mc_options(B = 20)
mc_options()$B

## restore orgiginal values
mc_options(o)
mc_options()$B
```
