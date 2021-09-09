#shiny::runApp("inst/shiny")

## install/update moosecounter package as needed
## need to install from github for rsconnect to work properly
#remotes::install_github("psolymos/moosecounter")

library(moosecounter)
library(shiny)
library(shinydashboard)
library(DT)
library(shinyBS)
library(shinyjs)
library(ggplot2)
#library(openxlsx)
#library(reactable)

ver <- read.dcf(
    file = system.file("DESCRIPTION", package = "moosecounter"),
    fields = "Version")

# Columns representing metadata, if present
var_meta <- c("SURVEY_NAM", "YT_REGION", "SURVEY_YEA", "SURVEY_ID",
              "S_SET_ID", "SU_ID", "ID", "SUS_", "SUS_ID",
              "In1Out0", "SU_STRATUM", "ALL_STRATA",
              "IDLATDEG", "IDLATMIN", "IDLONDEG", "IDLONMIN", "CENTRLAT", "CENTRLON",
              "REGION", "GMU", "GMU2", "USE_SCALE", "SRC_SCALE",
              "AREA_KM", "Sampled", "srv")

# Columns representing response variables
var_resp <- c("BULL_SMALL", "BULL_LARGE", "LONE_COW", "COW_1C", "COW_2C", "LONE_CALF",
              "UNKNOWN_AG", "MOOSE_TOTA", "COW_TOTA")

# Columns which can be filtered, if present
var_filter <- c("Survey Name" = "SURVEY_NAM",
                "Yukon Territory Region" = "YT_REGION",
                "Survey Year" = "SURVEY_YEA",
                "Survey ID" = "SURVEY_ID")

opts_tooltip <- list(
  "method" = "Defines the optimization algorithm used by <code>optim</code>",
  "response" = "Whether to use all moose spotted (Total) or just Cows",
  "maxcell" = "",
  "b" = paste("Number of times to run a simulation when establishing ",
              "prediction intervals"),
  "alpha" = paste("Type I error rate for prediction intervals,",
                  "as well as for multivariate exploration"),
  "wscale" = "How much to weight influential observations. 0 produces equal weights",
  "sightability" = "Detection probability used to correct total Moose results")

