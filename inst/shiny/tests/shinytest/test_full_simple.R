app <- ShinyDriver$new("../../", seed = 4323)
app$snapshotInit("test_full_simple")


# Settings ----------------------------------------------------------------
app$setInputs(menu = "settings")
app$setInputs(opts_b = 100)
app$snapshot()


# Load data ---------------------------------------------------------------
app$setInputs(menu = "data")
# This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(survey_file = "../../../extdata/MayoMMU_QuerriedData.csv")
app$snapshot()
app$setInputs(data_panel = "Data Structure")
app$snapshot()

# Explore Univariate ------------------------------------------------------
app$setInputs(sidebarItemExpanded = "Explore")
app$setInputs(menu = "univar")
app$setInputs(uni_var = "Fire1982_2012")
app$snapshot()
app$setInputs(uni_dist = "NB")
app$snapshot()
app$setInputs(uni_dist = "ZIP")
app$snapshot()
app$setInputs(uni_dist = "ZINB")
app$snapshot()

# Explore Multivariate ------------------------------------------------------
app$setInputs(menu = "multivar")
app$setInputs(multi_var = c("NALC_Shrub", "NALC_Needle", "Fire8212_DEM815"))
app$snapshot()
app$setInputs(multi_alpha = 0.999)
app$snapshot()
app$setInputs(multi_alpha = 0.001)


# Models ------------------------------------------------------------------
app$setInputs(sidebarItemExpanded = "Total")
app$setInputs(menu = "addmodel")
app$setInputs(model_var_count = c("Fire1982_2012", "NALC_Needle", "Subalp_Shrub_250buf"))
app$setInputs(model_add = "click")
app$setInputs(model_var_zero = c("ELCSub_Fire8212DEM815", "NALC_Shrub", "NALC_Needle"))
app$setInputs(model_add = "click")
app$snapshot()

app$setInputs(menu = "residuals")
app$snapshot()
app$setInputs(resid_model = "B")
app$snapshot()


# Prediction Intervals ----------------------------------------------------
app$setInputs(menu = "pi")

# Test prediction intervals with A/B
app$setInputs(pred_models = c("A", "B"))
app$setInputs(pred_calc = "click")
app$snapshot()

# Test with non-averaged
app$setInputs(pred_average = "FALSE")
app$setInputs(pred_calc = "click")
app$snapshot()

# Test figures
app$setInputs(pi_panel = "Moose Predictions")
app$snapshot()
app$setInputs(pred_cell = 600)
app$snapshot()
app$setInputs(pi_panel = "Full Bootstraps")
app$snapshot()
