#shiny::runApp("inst/shiny")

## install/update moosecounter package as needed
## need to install from github for rsconnect to work properly
#remotes::install_github("psolymos/moosecounter")

library(shiny)
library(shinydashboard)
library(shinyBS)
library(plotly)
library(openxlsx)
library(moosecounter)
library(knitr)
library(ggplot2)
library(reactable)

ver <- read.dcf(
    file = system.file("DESCRIPTION", package = "moosecounter"),
    fields = "Version")

#FooterText <- "<p>Shiny app made by the <a href='https://github.com/psolymos/moosecounter'>moosecounter</a> R package.</p>"
FooterText <- ""

