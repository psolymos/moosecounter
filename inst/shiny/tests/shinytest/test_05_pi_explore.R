app <- ShinyDriver$new("../../")
app$snapshotInit("test_05_pi_explore")


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
app$setInputs(pred_models = "A")
app$setInputs(pred_calc = "click")
app$snapshot()

# Explore PIs ----------------------------------------------------
app$setInputs(menu = "pi_map")
app$waitForValue("pred_map", iotype = "output")

# Different map fills
app$setInputs(pred_col = "fitted_values", timeout_ = 7000)
app$setInputs(pred_col = "Cell.mean", timeout_ = 7000)
app$setInputs(pred_col = "Cell.mode", timeout_ = 7000)
app$setInputs(pred_col = "Cell.pred", timeout_ = 7000)
app$setInputs(pred_col = "Cell.PIL", timeout_ = 7000)
app$setInputs(pred_col = "Cell.PIU", timeout_ = 7000)
app$setInputs(pred_col = "Cell.accuracy", timeout_ = 7000)
app$setInputs(pred_col = "Residuals", timeout_ = 7000)

# Selecting
app$setInputs(pred_data_rows_selected = 1, allowInputNoBinding_ = TRUE)
app$setInputs(pred_data_rows_selected = c(1, 100, 200), allowInputNoBinding_ = TRUE)
