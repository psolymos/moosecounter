#shiny::runApp("inst/shiny")

## install/update moosecounter package as needed
## need to install from github for rsconnect to work properly
#remotes::install_github("psolymos/moosecounter")

library(moosecounter)
library(shiny)
library(shinydashboard)
library(shinyBS)

library(dplyr)
library(DT)
library(ggiraph)
library(ggplot2) # Requires package mapproj for coord_map()
library(kableExtra)
library(openxlsx)
library(purrr)
library(patchwork)

options(DT.options = list(scrollX = TRUE))

ver <- read.dcf(
    file = system.file("DESCRIPTION", package = "moosecounter"),
    fields = "Version")

# Columns representing metadata, if present
var_meta <- c("SURVEY_NAM", "YT_REGION", "SURVEY_YEA", "SURVEY_ID",
              "S_SET_ID", "SU_ID", "ID", "SUS_", "SUS_ID", "S_YEAR_ID",
              "S_TYPE", "S_SEASON",
              "PROJECT_ID", "CENSUS_ID",
              "In1Out0", "SU_STRATUM", "ALL_STRATA",
              "IDLATDEG", "IDLATMIN", "IDLONDEG", "IDLONMIN", "CENTRLAT", "CENTRLON",
              "REGION", "MMU_ID", "GMU", "GMU2", "USE_SCALE", "SRC_SCALE", "SRC_NOTES",
              "AREA_KM", "PERIM_KM", "AREA_MI", "PERIM_MI", "FEATURE_ID",
              "SUBSET_NAM", "SUBSET_ID",
              "Sampled", "srv", "Kluane_ID")

# Columns representing response variables
var_resp <- c("BULL_SMALL", "BULL_LARGE", "LONE_COW", "COW_1C", "COW_2C", "LONE_CALF",
              "UNKNOWN_AG", "MOOSE_TOTA", "COW_TOTA")

# Columns which can be filtered, if present
var_filter <- c("Survey Name" = "SURVEY_NAM",
                "Yukon Territory Region" = "YT_REGION",
                "Survey Year" = "SURVEY_YEA",
                "Survey ID" = "SURVEY_ID")

# Columns which can be subsetted (during PI exploration/summaries)
var_subset <- c("MMU_ID", "SURVEY_NAM", "YT_REGION", "SURVEY_YEA", "SURVEY_ID",
                "PROJECT_ID", "CENSUS_ID",
                "REGION", "GMU", "GMU2",
                "SUBSET_NAM", "SUBSET_ID")


opts_tooltip <- list(
  "method" = "Defines the optimization algorithm used by <code>optim</code> for <emph>Prediction Intervals</emph>",
  "response" = "Whether to use all moose spotted (Total) or just Cows",
  "maxcell" = "Prediction values will be truncated at this value",
  "b" = paste("Number of times to run a simulation when establishing ",
              "Prediction Intervals"),
  "alpha" = "Type I error rate for prediction intervals",
  "wscale" = "How much to weight influential observations. 0 produces equal weights",
  "sightability" = "Detection probability used to correct total Moose results",
  "seed" = paste("Random seed. Can be changed if running into model errors, ",
                 "but keep note for replicability and error-reporting."))

opts_description <- list(
  "method" = "Optimization algorithm",
  "response" = "Response variable",
  "MAXCELL" = "Max total abundance in cells",
  "MINCELL" = "Min abundance for composition in cells",
  "B" = "Number of bootstrap runs",
  "alpha" = "Type I error rate for PI",
  "wscale" = "Weight",
  "sightability" = "Sightability",
  "seed" = "Random seed",
  "Ntot"="",
  "srv_name"="Filtering variable",
  "srv_value"="Filtering value",
  "area_srv"="Comun indicating survey areas",
  "Area"="Area",
  "xy"="Long/Lat",
  "composition"="Composition variables")

survey_tooltip <- list(
  "omit" = "These variables are potentially categorical (either text or integer) for which some levels are missing from surveyed data. Therefore predictions cannot be made they should be omitted. Override this by removing variables from this list.")


select_explanatory <- function(id, name, x, multiple = FALSE) {
  req(x)
  opts <- names(x)
  opts <- opts[!opts %in% c(var_meta, var_resp)]
  if(!multiple) opts <- c("none", opts)
  selectInput(id, name, opts, multiple = multiple)
}
