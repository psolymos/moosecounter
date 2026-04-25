# Total Estimate with Gassaway Formula

We do the stratification BEFORE the surveys are done using the fixed
wing aircraft. Every cell is stratified as LOW or HIGH. Then some of the
LOW and some of HIGH are surveyed to find how many moose are there.

## Usage

``` r
mc_gassaway(y1, y2, N1, N2)

# S3 method for class 'mc_gassaway'
print(x, ...)
```

## Arguments

- y1, y2:

  surveyed cell-level population counts in the 2 strata

- N1, N2:

  total number of cells that were stratified

- x:

  object to print

- ...:

  arguments passed to the print method

## Examples

``` r
y1 <- rpois(20, 50)
y2 <- rpois(30, 5)
N1 <- 40
N2 <- 50
mc_gassaway(y1, y2, N1, N2)
```
