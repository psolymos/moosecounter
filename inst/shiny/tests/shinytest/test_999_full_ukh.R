app <- ShinyDriver$new("../../")
app$snapshotInit("test_999_full_ukh")


# Settings ----------------------------------------------------------------
app$setInputs(menu = "settings")
app$setInputs(opts_b = 100)
app$snapshot()


# Load data ---------------------------------------------------------------
app$setInputs(menu = "data")
# This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(survey_file = "../../../extdata/UKH_2017_QuerriedSU_Final_Corrected_for_Analysis.csv")
app$snapshot()

# Explore Univariate ------------------------------------------------------
app$setInputs(sidebarItemExpanded = "Explore")
app$setInputs(menu = "univar")
app$setInputs(uni_var = "Fire8212_DEM815_Sub")
app$snapshot()

# Explore Multivariate ------------------------------------------------------
app$setInputs(menu = "multivar")
app$setInputs(multi_var = c("Fire8212_DEM815_Sub", "NALC_AllNeedleWet", "DShMiFires815_Sub"))
app$snapshot()


# Models ------------------------------------------------------------------
app$setInputs(sidebarItemExpanded = "Total")
app$setInputs(menu = "addmodel")
app$setInputs(model_var_count = "Fire8212_DEM815_Sub")
app$setInputs(model_add = "click")
app$setInputs(model_var_zero = c("NALC_AllNeedleWet", "DShMiFires815_Sub"))
app$setInputs(model_add = "click")
app$snapshot()

app$setInputs(menu = "residuals")
app$setInputs(resid_model = "B")
app$snapshot()


# Prediction Intervals ----------------------------------------------------
app$setInputs(menu = "pi")

# Test prediction intervals with A/B/C
app$setInputs(pred_models = c("A", "B"))
app$setInputs(pred_calc = "click", timeout_ = 10000)

# Test with non-averaged
app$setInputs(pred_average = "FALSE")
app$setInputs(pred_calc = "click", timeout_ = 7000)

# Test figures
app$setInputs(pred_cell = 2)
app$setInputs(pred_cell = 14)
app$setInputs(pred_cell = 150)
app$snapshot()

# Explore PIs ----------------------------------------------------
app$setInputs(menu = "pi_map")
app$waitForValue("pred_map", iotype = "output")

# Selecting
app$setInputs(pred_data_rows_selected = c(1, 100, 200), allowInputNoBinding_ = TRUE)

# No snapshots for PI explore because of massive map data which is tough to compare
