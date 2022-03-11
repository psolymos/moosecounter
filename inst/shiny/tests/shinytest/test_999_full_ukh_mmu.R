app <- ShinyDriver$new("../../")
app$snapshotInit("test_999_full_ukh_mmu")


# Settings ----------------------------------------------------------------
app$setInputs(menu = "settings")
app$setInputs(opts_b = 100)
app$snapshot()


# Load data ---------------------------------------------------------------
app$setInputs(menu = "data")
# This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(survey_file = "../../../extdata/UKH2017_MMU_Querried_data_for_Analysis_Final_3.csv")
app$snapshot()

# Explore Univariate ------------------------------------------------------
app$setInputs(sidebarItemExpanded = "Total")
app$setInputs(menu = "total_univar")
app$setInputs(uni_var = "Fire1982to2012")
app$snapshot()

# Explore Multivariate ------------------------------------------------------
app$setInputs(menu = "total_multivar")
app$setInputs(multi_alpha = 0.1)
app$setInputs(multi_var = c("Fire1982to2012", "NALC_DesShrMix", "Fire8212_DEM800to1200"))
app$snapshot()


# Models ------------------------------------------------------------------
app$setInputs(menu = "total_models")
app$setInputs(total_model_dist = "P")
app$setInputs(total_model_var_count = "Fire1982to2012")
app$setInputs(total_model_add = "click")
app$setInputs(total_model_var_zero = c("NALC_DesShrMix", "Fire8212_DEM800to1200"))
app$setInputs(total_model_add = "click")
app$snapshot()

app$setInputs(menu = "total_residuals")
app$setInputs(total_resid_model = "B")
app$snapshot()


# Prediction Intervals ----------------------------------------------------
app$setInputs(menu = "total_pi")

# Test prediction intervals with A/B/C
app$setInputs(total_pi_models = c("A", "B"))
app$setInputs(total_pi_calc = "click", timeout_ = 10000)

# Test with non-averaged
app$setInputs(total_pi_average = "FALSE")
app$setInputs(total_pi_calc = "click", timeout_ = 7000)

# Test figures
app$setInputs(total_pi_cell = 2)
app$setInputs(total_pi_cell = 14)
app$setInputs(total_pi_cell = 150)
app$snapshot()

# Explore PIs ----------------------------------------------------
app$setInputs(menu = "total_pi_map")
app$waitForValue("total_pi_map", iotype = "output")

# Selecting
app$setInputs(total_pi_data_rows_selected = c(1, 100, 200), allowInputNoBinding_ = TRUE)

# No snapshots for PI explore because of massive map data which is tough to compare
