# Internal Functions: Models

These functions power the total Moose estimation and prediction.

## Usage

``` r
wzi(object, pass_data = FALSE, ...)

loo(object, ...)

zeroinfl2(
  formula,
  data,
  subset,
  na.action,
  weights,
  offset,
  dist = "ZIP",
  link = c("logit", "probit", "cloglog"),
  control = NULL,
  model = TRUE,
  y = TRUE,
  x = FALSE,
  solveH = TRUE,
  robust = FALSE,
  ...
)

# S3 method for class 'non_zeroinfl'
summary(object, ...)

# S3 method for class 'summary.non_zeroinfl'
print(x, digits = max(3, getOption("digits") - 3), ...)

# S3 method for class 'zeroinfl'
nobs(object, ...)

# S3 method for class 'hurdle'
nobs(object, ...)
```

## Arguments

- object:

  A model as returned by \`zeroinfl2()\`.

- pass_data:

  Logical, to pass the data or not.

- ...:

  Other parameters passes to underlying functions.

- formula:

  Model formula as in \`pscl::zeroinfl()\`.

- data:

  Moose data frame.

- subset, na.action, weights, offset, model, y:

  Arguments as in \`pscl::zeroinfl()\`.

- dist:

  Count distribution, one of \`"ZIP"\`, \`"ZINB"\`, \`"P"\`, \`"NB"\`.

- link:

  Link function for the zero model.

- control:

  See \`pscl::zeroinfl.control()\`.

- x:

  Arguments for \`pscl::zeroinfl()\` or a model as returned by
  \`zeroinfl2()\` for the methods.

- solveH:

  Logical, to use robust matrix inversion to get VCV.

- robust:

  Logical, use CL/PL robust regression approach.

- digits:

  Digits for print method.

## Details

\`zeroinfl2\` is a customized version of the \`pscl::zeroinfl()\`
function, but this also fits the non-ZI counterparts in a way that
simplifies downstream analyses (i.e. PI calculations). Intended for
internal use.

\`wzi\` applies a leave-one-out approach to temper influential
observations. The process finds weights that are related to leverage
(how much each observation contributes to the model likelihood).

The robust version (CL/PL = conditional likelihood / pseudo likelihood)
applies the conditioning described in Solymos et al. (2021)
\<https://doi.org/10.1002/env.1149\>.
