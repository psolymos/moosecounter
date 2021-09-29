app <- ShinyDriver$new("../../")
app$snapshotInit("test_04_pi")


# Settings ----------------------------------------------------------------
app$setInputs(menu = "settings")
app$setInputs(opts_b = 100)

# Load data ---------------------------------------------------------------
app$setInputs(menu = "data")
# This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(survey_file = "../../../extdata/MayoMMU_QuerriedData.csv")

# Models ------------------------------------------------------------------
app$setInputs(sidebarItemExpanded = "Total")
app$setInputs(menu = "addmodel")
app$setInputs(model_var_count = "NALC_Needle")
app$setInputs(model_add = "click")

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
app$setInputs(pi_panel = "Bootstrap Results")
app$snapshot()
