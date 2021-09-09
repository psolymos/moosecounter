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
