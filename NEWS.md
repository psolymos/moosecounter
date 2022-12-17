# Version 0.6-2 -- December 17, 2022

* Added Residual tab for composition analysis to show model summaries and AIC table.
* More columns are allowed to be treated as filters/subsetters with 2 levels (usually 0/1).
* Print abundance/density summaries for subsets under Summary tab.
* Added `Total_Bulls` and `Bulls_per_Cow` to the composition summaries.

# Version 0.6-1 -- December 14, 2022

* Composition table contains BIC and coefficients.
* Composition checks fixed: don't include unknown ages.
* `mc_plot_univariate` has the ability to return ggplot2 objects and their interactive versions.
* New function: `mc_plot_predfit` to check model fit.
* Shiny app: predictions can now be subsetted independent of the training data.

# Version 0.6-0 -- March 14, 2022

* Composition analysis added to R package and Shiny app.

# Version 0.5-0 -- October 4, 2021

* Total moose estimation is tested and ready to be used in the field.
* Added `run_app()` function to launch a Shiny app.

# Version 0.4-0 -- August 19, 2021

* Package renamed from DeducerPlugInMoose to moosecounter,
  with the intention of dropping the rJava/Deducer GUI features
  in favor of a Shiny app. Version numbering is continuous.

# Version 0.3-3 -- March 24, 2020

* Composition analysis fix: vglm failed when input matrix rowsum was 0.
  Now it is treated as missing and omitted.

# Version 0.3-1 -- July 2, 2019

* Added `nobs.zeroinfl` method.

# Version 0.3-0 -- March 24, 2019

* Fixing Non-ZI model summaries.
* Increment version.

# Version 0.2-16 -- January 10, 2019

* Non-ZI versions of count models (P and NB) added by introducing
  a hacked version of zeroinfl: zeroinfl2.

# Version 0.2-15 -- December 19, 2018

* Global option `wscale` added to tune weighting scale.

# Version 0.2-14 -- November 10, 2018

* Dual (weighted & unweighted) prediction added to composition PI
  calculations.

# Version 0.2-13 -- November 1, 2018

* Dual (weighted & unweighted) prediction is performed depending
  on survey area (unsurveyed gets predicted under unweighted
  model to better represent the high end of PI, surveyed area
  gets predicted under weighted model because high values
  are already captured within surveyed cells).

# Version 0.2-12 -- October 21, 2018

* Write down PI/CPI algorithm
* Accumulate issues and report as part of the summaries

# Version 0.2-11 -- October 15, 2018

* Error catching fixes in CPI calculation.

# Version 0.2-10 -- October 14, 2018

* Error catching fixes in PI calculation.

# Version 0.2-9 -- October 2, 2018

* Exposed optim method through options, might be a good idea to set it
  to Nelder-Mead (because of weighting instability using BFGS).

# Version 0.2-8 -- August 30, 2018

* Fixed issue with `plotResiduals` and `plot_predPI`: 
  failed when there were no outliers with error 
  `zero-length 'labels' specified`.

# Version 0.2-7 -- April 18, 2018

* PI distribution plot is tweaked to display sensible results for the case
  when the distribution has only one unique value (i.e. 0s).

# Version 0.2-6 -- April 4, 2018

* ZIP added as option beside ZINB (univariate exploration and model fitting).
* Weighted modeling option added to minimize influence on predictions.
* Histograms show % instead of density (%=100*density).
* Residual plot labels +/- 1.5*SD points and uses symmetric divergent coloring.

# Version 0.2-5 -- March 17, 2018

* Cell level stats now include Mode as well.

# Version 0.2-4 -- March 15, 2018

* ZI prediction did not use covariate coefs, only the intercept
  that led to biased simulations.
* Model averaging now uses selected models from dialog instead of all
  model in the ModelTab
* Explicitly imports fitted and model.frame methods from VGAM for
  composition analysis to avoid scoping issues with formula
* Number of hist bins in PlotPiDistr can be set by the user.
* Added AIC weights to composition model table.
* Composition PI now has the option for model selection similar to
  total Moose PI.

# Version 0.2-3 -- March 12, 2018

* Fixed alpha level: was not available for some functions from options.
* Null ZINB model in model dialogue fixed (length=0, not =="")

# Version 0.2-2 -- February 28, 2018

* Allow ZI to vary with covariates, UI and help dialog updated.
  PI simulations how use the model with ZI covariates as well.
* Model averaging function tracks the fitted values and uses
  the model averaged mean as the fit.
* Started adding Rd files with function documentations.

# Version 0.2-1 -- February 20, 2018

* Added multi-model averaging to PI simulation:
  use weights from model table to select models to refit
  and the option is added to the UI dialog.
* Checked and updated help dialogues to reflect updates.
* Added mode to total moose PI table.
* Added plot of PI distribution (PI) to UI: 
  called 'Plot pred. distribution' in the menu,
  option to select full or subset PI data.
* pb option set to "none" (tcltk froze Mac).

# Version 0.2-0 -- March 31, 2017

* Bootstrap mean with 2 decimal places added to PiData table.
* Unknown animals are dropped from compositional analysis with a warning.
* Sightability correction can be defined up front, defaults to 1.
* Summary tables print decimal numbers instead of scientific notation.
* Total adult cows can be used as response for total models.
* Calf/cow ratio and related compositional analysis added.

# Versions 0.1-1 -- 0.1-5: April 6 -- July 9, 2016

Incremental production versions.

# Version 0.1-0: March 11, 2016

Initial release
