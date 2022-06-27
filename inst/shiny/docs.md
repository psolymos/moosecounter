## General workflow

The over all workflow is to set **Settings**, load your **Data**, model
**Total** moose counts and then model moose **Composition**.

Within the two modelling steps are further steps: **Explore** the
variables, create **Models**, check model **Residuals** (in Total),
create **Prediction Intervals** and then **Explore** or **Summarize**
these prediction intervals.

Next, we will go over these steps in detail.

------------------------------------------------------------------------

## Settings

You can adjust the settings for the analyses by modifying options:

-   response (total vs. cows),
-   model optimization methods (Nelder-Mead, etc.)
-   bootstrap iterations
-   alpha level
-   weighting scale
-   sightability
-   random seed\*

\* particularly useful for troubleshooting and reproducibility

At the bottom of the settings page is a section, Current Settings, which
you can peruse to see all settings in effect, including those not
currently available for modification.

> Note that for reproducibility and troubleshooting, settings are
> included in the data downloads (under **Total &gt; Explore
> Predictions** and **Composition &gt; Summary**)

------------------------------------------------------------------------

## Data

To start an analysis, you’ll first want to load your data. We include an
[example data set](MayoMMU_QuerriedData.csv) if you would like to
explore but do not have your own data yet. Click on “Browse” to choose a
data set.

**Explore** your data using Interactive Table and Data Structure tabs to
ensure the data you’ve loaded looks as it should.

> Note that predictors/explanatory variables are *inferred* and defined
> as variables which are **not** Response variables or Metadata
> variables. See the Data Structure tab so see how your variables have
> been categorized.
>
> Potential response variables are: `BULL_SMALL`, `BULL_LARGE`,
> `LONE_COW`, `COW_1C`, `COW_2C`, `LONE_CALF`, `UNKNOWN_AG`,
> `MOOSE_TOTA`, `COW_TOTA`
>
> Potential metadata variables are: `SURVEY_NAM`, `YT_REGION`,
> `SURVEY_YEA`, `SURVEY_ID`, `S_SET_ID`, `SU_ID`, `ID`, `SUS_`,
> `SUS_ID`, `S_YEAR_ID`, `S_TYPE`, `S_SEASON`, `PROJECT_ID`,
> `CENSUS_ID`, `In1Out0`, `SU_STRATUM`, `ALL_STRATA`, `IDLATDEG`,
> `IDLATMIN`, `IDLONDEG`, `IDLONMIN`, `CENTRLAT`, `CENTRLON`, `REGION`,
> `GMU`, `GMU2`, `USE_SCALE`, `SRC_SCALE`, `SRC_NOTES`, `AREA_KM`,
> `PERIM_KM`, `AREA_MI`, `PERIM_MI`, `FEATURE_ID`, `SUBSET_NAM`,
> `SUBSET_ID`, `Sampled`, `srv`, `Kluane_ID`

**Omit variables with too few surveyed levels.**

In order for a predictor to be useful, it’s levels must exist in cells
that were surveyed as well as in cells that were not. If they don’t have
this variability they can cause problems when modelling. While you can
simply ignore these variables in the models, it can be simpler to omit
them here. Any variables that may be problematic will be automatically
selected. You may choose to *not* omit them if you wish, by removing
them from this selection.

**Convert integer to categorical**

When R loads data scored as numbers, it assumes they are numeric.
However, depending how you’ve created your data, integer values may
actually be categorical. Click on the “Data Structure” tab to see what
the current format of your data is. To ensure your models are
appropriate, add any variable which should be categorical but are
instead listed as integer (`int`) to this field. They should then switch
to categorical (`Factor`) in the Data Structure display.

**Filter data**

If you wish to model only a subset of your data, choose the appropriate
grouping levels in this section. Then use the Interactive Table and Data
Strucutre tabs to double check your selection.

------------------------------------------------------------------------

## Modelling moose totals

### Exploring predictors

Before modelling you should explore your potential predictors either
one-by-one (**Univariate** exploration) or in tandem (**Multivariate**
exploration).

In a univariate exploration, choose the variable of interest and the
distribution you want to model. The figures below reflect Total Moose
Counts as a function of the variable chosen by density of samples,
spatial sampling, and the total moose response.

In a multivariate exploration, choose at least two variables of interest
and the alpha level you want to use to define a split. The figure below
shows the conditional inference tree (non-parametric regression tree)
defining relationships among the variables.

### Build models

In the Models window you can build different models to explore. You can
assign variables to be modelled as count or zero or both. You can choose
the distribution family to model (**P** = Poisson, **NB** = Negative
Binomial, **ZIP** = Zero Inflation Poisson, and **ZINB** = Zero
Inflation Negative Binomial), and whether the variables should be
weighted or not (weighting moderates potentially influential
observations). By default, models are named alphabetically, but you can
give each model a distinct ID if you choose.

Once you have added models, they will appear in the model table and in
the AIC Model Comparison table. You can delete models by clicking on the
“X” next to them.

Any model problems (convergence, etc.) will be displayed in the Error
messages section.

### Exploring Residuals

The Residuals window will show you the AIC Model Comparison table along
with plots showing both the spatial (left) and general (right)
distributions of residuals for the selected model.

You can compare model fits and, if necessary, remove models by clicking
on the X next to the model name in the Models window.

### Prediction Intervals

After you’ve settled on the models you want to use, go to the Prediction
Intervals window to calculate the PIs.

**Choosing Models**

First choose the model(s) you want to use. If you choose more than one,
you have the option of using the best among those models, or averaging
over the models. Once you’re satisfied, click the “Calculate PI” button.
Calculating the prediction intervals can take some time, watch the
progress bar in the lower right hand corner to see how it goes.

> Note that if you change anything used to calculate Prediction
> Intervals (Settings, Data, Models), the button will turn Yellow and
> prompt you to re-run the calculation.

**Outputs**

The PI output has several different parts. First there is the overall
“Summary” of estimated Total moose counts and density. You are reminded
of the settings and alerted to potential issues under “Issues and
Options”. In the lower half of the window you have the model outputs.
These are divided in to sub tabs.

The first tab is Diagnostic Plots. As in the Residuals window, we have
plots showing spatial (left) and general (centre) distributions of
residuals. We also have a third plot (right) showing spatial accuracy.
The model used is noted in the title of these plots. If you chose to
average over models, it will have “Avg” as the title.

The second tab is Moose Predictions. This tab shows density plots of the
prediction intervals over all (left) and for specific cells in the data
(right). Use the new input in the upper right box called “Cell to plot
for predictions” to choose a cell to plot.

> If the right-hand, cell-level plot is empty, with text such as
> “Observed Count = X”, this means that the cell was actually surveyed,
> so there is a single observation and no prediction for that cell.

The third tab is Bootstrap Results. This shows an interactive table of
bootstrap results per `SU_ID`.

> Note that these data are all available to download as sheets in an
> Excel file in the next window, Explore Predictions.

### Explore Predictions

In this window, you can subset the data you want to explore by specifying the 
column defining the group and then the groups you want to keep. By default all
data is displayed (**this only applies to the display, the download contains all
data**). 

For the map, you can pick different calculated values to display. 
You can also adjust the number of colour bins to ensure the best
differentiation.

For example, the starting value shows you “observed values” which are
the actual values surveyed. “Cell.pred” shows you the predicted moose
counts based on your models, which may help identify un-surveyed
locations with potential high numbers of moose.

You can download the run info, settings, data, and bootstrap runs as an
Excel file by clicking “Download full results as Excel file”.

------------------------------------------------------------------------

## Modelling moose composition

### Explore

As with the Total count models, you should first explore your potential
predictors. Use the drop down menu to choose a predictor to explore. The
figures show moose composition as a function of the total moose count.

### Models

Next, build your models by defining the model ID (defaults to letters of
the alphabet) and the predictors you want to include. As with Totals,
you can define several models and compare them with the AIC table.

You can delete models by clicking on the X next to the Model ID. Error
messages, if any, will appear in the lower left corner.

### Prediction Intervals

**Choosing Models**

To calculate Prediction Intervals for Composition, you will need models
for **Total** counts as well as models for composition. Total count
models can be averaged, therefore you can select more than one. However,
you can only select one composition model. As in **Total &gt; Prediction
Interval**, if you select more than one Total model you can decide
whether you want to use the best model or average over the models.

Once you are satisfied with your selection, click “Calculate PI”. Again
this may take some time, see the progress bar in the lower right corner.

> Note that if you change anything used to calculate Prediction
> Intervals (Settings, Data, Total Models, Composition Models), the
> button will turn Yellow and prompt you to re-run the calculation.

**Outputs**

Potential issues and options are defined in the lower left corner, while
the Summary and Bootstrap Results are presented as tabs on the right
side of the window.

The Summary table shows the percentile for the prediction intervals of
total moose counts as well as by composition.

The Bootstrap Results show an interactive table of bootstrap results per
`SU_ID`. (This table may take time to load).

### Summary

Finally we have the Composition Summary window. Here you can see the
prediction intervals for total moose counts and composition by `SU_ID`.

You can subset the data you want to explore by specifying the 
column defining the group and then the groups you want to keep. By default all
data is displayed (**this only applies to the display, the download contains all
data**). 

You can download the run info, settings, data, and bootstrap runs as an
Excel file by clicking “Download full results as Excel file
