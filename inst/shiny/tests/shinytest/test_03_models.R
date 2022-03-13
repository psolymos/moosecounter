app <- ShinyDriver$new("../../")
app$snapshotInit("test_03_models")


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
app$setInputs(total_model_var_count = c("Fire1982_2012", "NALC_Needle", "Subalp_Shrub_250buf"))
app$setInputs(total_model_add = "click")
app$setInputs(total_model_var_zero = c("ELCSub_Fire8212DEM815", "NALC_Shrub", "NALC_Needle"))
app$setInputs(total_model_add = "click")
app$snapshot()

app$setInputs(menu = "total_residuals")
app$snapshot()
app$setInputs(total_resid_model = "B")
app$snapshot()
