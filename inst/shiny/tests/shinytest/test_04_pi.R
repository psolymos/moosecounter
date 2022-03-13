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
app$setInputs(menu = "total_models")
app$setInputs(total_model_dist = "P")
app$setInputs(total_model_var_count = "NALC_Needle")
app$setInputs(total_model_add = "click")

# Prediction Intervals ----------------------------------------------------
app$setInputs(menu = "total_pi")

# Test prediction intervals with A/B
app$setInputs(total_pi_models = c("A", "B"))
app$setInputs(total_pi_calc = "click")
app$snapshot()

# Test with non-averaged
app$setInputs(total_pi_average = "FALSE")
app$setInputs(total_pi_calc = "click")
app$snapshot()

# Test figures
app$setInputs(total_pi_panel = "Moose Predictions")
app$snapshot()
app$setInputs(total_pi_cell = 600)
app$snapshot()
app$setInputs(total_pi_panel = "Bootstrap Results")
app$snapshot()
