---
title: docs test
output: 
  html_document:
    css: "www/styles.css"
---

### General workflow

The over all workflow is to set **Settings**, load your **Data**, model **Total**
moose counts and then model moose **Composition**. 

Within the two modelling steps are further steps: **Explore** the variables, 
create **Models**, check model **Residuals** (in Total), create 
**Prediction Intervals** and then **Explore** or **Summarize** 
these prediction intervals.

Next, we will go over these steps in detail.

---

### Settings
You can adjust the settings for the analyses by modifying options in the settings
tab. Settings which you can modify include:

- response (total vs. cows only), 
- model optimization methods (Nelder-Mead, etc.)
- bootstrap iterations
- alpha level
- weighting scale
- sightability
- random seed

At the bottom of the settings page is a section, "Current Settings" which you can 
peruse to see all settings in effect, including those not currently available for 
modification.

> Note that for reproducibility and troubleshooting, settings are included 
> in the data downloads (under **Total > Explore Predictions** and **Composition > Summary**)

---

### Data

#### Load data

To start an analysis, you'll first want to load your data under the "Data" tab. 
We include an [example data set](MayoMMU_QuerriedData.csv) if you would like to 
explore but do not have your own data yet. Click on "Browse" to choose a data set.

#### Explore data

Use the "Interactive table" or look at the "Data Structure" to ensure the data 
you've loaded looks as it should.

#### Fix data

**Omit variables with too few surveyed levels**

Sometimes variables may have too little variability in their categorical levels, 
which can cause problems when modelling. While you can simply ignore these variables
in the models, it can be simpler to omit them here.

**Convert integer to categorical**

When R loads data scored as numbers, it assumes they are numeric. However,
depending how you've created your data, they may actually be categorical. Click
on the "Data Structure" tab to see what the current format of your data is. To
ensure your models are appropriate, add any variable which should be categorical
but is instead listed as numeric (`num`) to this field. They should then switch
to categorical (`Factor`) in the Data Structure display.

#### Filter data

If you wish to model only a subset of your data, choose the appropriate grouping
levels in this section. Then use the Interactive Table to double check your 
selection.

---

### Modelling moose totals

#### Exploring predictors
Before modelling you will want to explore your potential predictors either 
one-by-one (**Univariate** exploration) or in tandem (**Multivariate** exploration). 

In a univariate exploration, choose the variable of interest and the distribution 
you want to model. The figures below reflect Total Moose Counts as a function of the 
variable chosen by density of samples, spatial sampling, and the total moose response. 

In a multivariate exploration, choose at least two variables of interest and the 
alpha level you want to use to define a split. The figure below shows the 
conditional inference trees (non-parametric regression trees) defining 
relationships among the variables. 

#### Defining models
In the Models tab you can define different models to explore. You can assign
variables to be modelled as count or zero or both. You can choose the distribution
family to model and whether the variables should be weighted or not (weighting
moderates potentially influential observations). By default, models are named
alphabetically, but you can give each model a distinct ID if you choose. 

Once you have added models, they will appear in the model table and in the AIC
Model Comparison table. You can delete models by clicking on the "X" next to them.

Any model problems (convergence, etc.) will be displayed in the Error messages
section. 



#### Exploring Residuals


---

### Modelling moose composition
