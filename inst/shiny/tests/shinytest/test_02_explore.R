app <- ShinyDriver$new("../../")
app$snapshotInit("test_02_explore")


# Settings ----------------------------------------------------------------
app$setInputs(menu = "settings")
app$setInputs(opts_b = 100)
app$setInputs(opts_seed = 4323, wait_=FALSE, values_=FALSE)

# Load data ---------------------------------------------------------------
app$setInputs(menu = "data")
# This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(survey_file = "../../../extdata/MayoMMU_QuerriedData.csv")

# Explore Univariate ------------------------------------------------------
app$setInputs(sidebarItemExpanded = "Total")
app$setInputs(menu = "total_univar")
app$setInputs(uni_var = "Fire1982_2012")
app$setInputs(uni_dist = "P")
app$snapshot()
app$setInputs(uni_dist = "NB")
app$snapshot()
app$setInputs(uni_dist = "ZIP")
app$snapshot()
app$setInputs(uni_dist = "ZINB")
app$snapshot()

# Explore Multivariate ------------------------------------------------------
app$setInputs(menu = "total_multivar")
app$setInputs(multi_var = c("NALC_Shrub", "NALC_Needle", "Fire8212_DEM815"))
app$setInputs(multi_alpha = 0.1)
app$snapshot()
app$setInputs(multi_alpha = 0.5)
app$snapshot()
app$setInputs(multi_alpha = 0.001)
