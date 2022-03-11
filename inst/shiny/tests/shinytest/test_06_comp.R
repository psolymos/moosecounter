app <- ShinyDriver$new("../../")
app$snapshotInit("test_06_comp")


# Settings ----------------------------------------------------------------
app$setInputs(menu = "settings")
app$setInputs(opts_b = 100)

# Load data ---------------------------------------------------------------
app$setInputs(menu = "data")
# This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(survey_file = "../../../extdata/MayoMMU_QuerriedData.csv")

# Total Models ---------------------------------------------------------------
app$setInputs(sidebarItemExpanded = "Total")
app$setInputs(menu = "total_models")
app$setInputs(total_model_var_count = "Fire8212_DEM815")
app$setInputs(total_model_add = "click")
app$setInputs(total_model_var_count = c("Fire8212_DEM815", "NALC_Needle", "Fire1982_2012"))
app$setInputs(total_model_add = "click")

# Composition ---------------------------------------------------------------
app$setInputs(sidebarItemExpanded = "Composition")
app$setInputs(menu = "comp_explore")
app$setInputs(comp_explore_var = "NALC_Shrub")
app$snapshot()

app$setInputs(menu = "comp_models")
app$setInputs(comp_model_var = c("Fire1982_2012", "NALC_Needle"))
app$setInputs(comp_model_add = "click")
app$snapshot()

app$setInputs(menu = "comp_pi")
app$setInputs(comp_pi_models1 = "A")
app$setInputs(comp_pi_calc = "click")
app$snapshot()

app$setInputs(menu = "comp_sum")
app$snapshot()

app$setInputs(menu = "comp_pi")
app$setInputs(comp_pi_models1 = c("A", "B"))
app$setInputs(comp_pi_calc = "click")
app$snapshot()

app$setInputs(menu = "comp_sum")
app$snapshot()

app$setInputs(menu = "comp_pi")
app$setInputs(comp_pi_models1 = c("A", "B"))
app$setInputs(comp_pi_average = "FALSE")
app$setInputs(comp_pi_calc = "click")
app$snapshot()
