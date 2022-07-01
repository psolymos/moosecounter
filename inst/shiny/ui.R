## note: the ui_* UI pieces can be stored & sourced as separate file
## or added as modules

# Home -------------------
ui_home <- fluidRow(
  column(width=12,
    includeMarkdown("intro.md")
  )
)

# Setting -------------------
ui_settings <- fluidRow(
  column(width=12,

         h2("Settings"),

         box(width = 12,
             column(width = 6,
             radioButtons("opts_response", inline = TRUE,
                          label = "Response variable",
                          choices = c("Total" = "total", "Cows" = "cows")),
             bsTooltip("opts_response", opts_tooltip$response),

             radioButtons("opts_method",  inline = TRUE,
                          label = "Model optimization method",
                          choices = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B",
                                      "SANN")),
             bsTooltip("opts_method", opts_tooltip$method),

             sliderInput("opts_b",
                         label = "No. bootstrap interations",
                         value = 500, min = 100, max = 1000, step = 50),
             bsTooltip("opts_b", opts_tooltip$b),

             radioButtons("opts_alpha", inline = TRUE,
                          label = "Alpha level",
                          choices = c(0.01, 0.05, 0.1),
                          selected = 0.1),
             bsTooltip("opts_alpha", opts_tooltip$alpha)
           ),

           column(width = 6,
              # sliderInput("opts_maxcell",
              #            label = "Maximum abundance in cells",
              #            min = 1, max = 100),
              # bsTooltip("opts_maxcell", opts_tooltip$maxcell),

             sliderInput("opts_wscale",
                         label = "Weighting scale",
                         value = 1, min = 0, max = 10, step = 0.1),
             bsTooltip("opts_wscale", opts_tooltip$wscale),

             sliderInput("opts_sightability",
                         label = "Sightability",
                         value = 1, min = 0, max = 1, step = 0.1),
             bsTooltip("opts_sightability", opts_tooltip$sightability),
             numericInput("opts_seed",
                          label = "Random seed",
                          value = 4323, min = 1, max = 10000),
             bsTooltip("opts_seed", opts_tooltip$seed)
           )),
         box(width = 12, title = "Current Settings",
             tableOutput("opts"))

         # Surveyed units?
         # Surveyed area?

  )
)

# Data -------------------
ui_data <- fluidRow(
  column(width = 12,
    h2("Data"),

    box(width = 4,
        fileInput("survey_file", "Choose Survey CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        uiOutput("survey_omit_ui"),
        bsTooltip("survey_omit", survey_tooltip$omit),
        uiOutput("survey_factors_ui"),
        h3("Filter data"),
        uiOutput("filters_ui")),

    box(width = 8,
        tabBox(width = 12, id = "data_panel",
          tabPanel("Interactive table",
                   div(style = "overflow-x: scroll", DTOutput("survey_preview"))),
          tabPanel("Data Structure",
                   h3("Explanatory variables",
                      span("(inferred)", style = "font-size: 70%;")),
                   verbatimTextOutput("survey_explanatory"),
                   h3("Response variables"),
                   verbatimTextOutput("survey_response"),
                   h3("Metadata variables"),
                   verbatimTextOutput("survey_meta"))))

  )
)

# Total --------------------------------------------------------------------
## Explore - Univariate ----------------
ui_total_univar <- fluidRow(
  column(width = 12,
         h2("Univariate Exploration"),
         box(height = "100px", uiOutput("uni_var_ui")),
         box(height = "100px",
             radioButtons("uni_dist", "Distribution", inline = TRUE,
                          choices = c("P", "NB", "ZIP", "ZINB"),
                          selected = "NB")),
         box(width = 12,
             column(width = 4, girafeOutput("uni_graph1", height = "100%")),
             column(width = 4, girafeOutput("uni_graph2", height = "100%")),
             column(width = 4, girafeOutput("uni_graph3", height = "100%"))
         )
  )
)

## Explore - Multivariate -------------------
ui_total_multivar <- fluidRow(
  column(width = 12,
         h2("Multivariate Exploration"),
         box(width = 6, uiOutput("multi_var_ui")),
         box(width = 6, sliderInput("multi_alpha", label = "alpha level for split",
                                    value = 0.01,
                                    min = 0.001, max = 0.5, step = 0.01)),
         box(width = 12, plotOutput("multi_graph"))
  )
)


## Add model -------------------
ui_total_models <- fluidRow(
  column(width = 12,
    h2("Add model"),
    column(width = 4,
           box(width = 12,
               uiOutput("total_model_id_ui"),
               uiOutput("total_model_var_count_ui"),
               uiOutput("total_model_var_zero_ui"),
               radioButtons("total_model_dist", "Distribution",
                            choices = c("P", "NB", "ZIP", "ZINB"),
                            selected = "NB",
                            inline = TRUE),
               radioButtons("total_model_weighted", NULL,
                            c("Non weighted" = FALSE, "Weighted" = TRUE),
                            inline = TRUE),
               bsButton("total_model_add", "Add", style = "primary")),
           box(width = 12,
               h4("Error messages"),
               uiOutput("total_model_msgs"))),
    box(width = 8,
        h4("Current models"),
        tableOutput("total_model_table"),
        uiOutput("total_model_delete_ui"),
        hr(),
        h4("AIC Model Comparison"),
        div(style = "overflow-x: scroll", tableOutput("total_model_aic1")))
  )
)

## Residuals -----------------
ui_total_residuals <- fluidRow(
  column(width = 12,
    h2("Residuals"),
    box(width = 12,
        h4("AIC Model Comparison"),
        div(style = "overflow-x: scroll", tableOutput("total_model_aic2")),
        uiOutput("total_resid_models_ui"),
        plotOutput("total_resid_plot"),
        verbatimTextOutput("total_resid_summary"))
  )
)

## Prediction Intervals -----------------
ui_total_pi <- fluidRow(
  column(width=12,
    h2("Calculating Prediction Intervals"),
    box(width = 4, height = "225px",
        column(width = 6,
               uiOutput("total_pi_models_ui"),
               bsButton("total_pi_calc", "Calculate PI",
                        style = "primary")),
        column(width = 6, conditionalPanel(
          condition = "input.total_pi_models.length > 1",
          radioButtons("total_pi_average", label = "With multiple models...",
                       choices = c("Use best model" = FALSE,
                                   "Average over models" = TRUE),
                       selected = TRUE)),
          uiOutput("total_pi_cell_ui")),
        hr(),
        uiOutput("total_pi_selected", style = "margin-top:50px")),

    box(width = 5, height = "225px",
        title = "Summary",
        tableOutput("total_pi_density")),

    box(width = 3, height = "225px",
        title = "Issues and Options",
        tableOutput("total_pi_options")),

    tabBox(width = 12, id = "total_pi_panel",
           tabPanel("Diagnostic Plots", plotOutput("total_pi_predpi")),
           tabPanel("Moose Predictions",
                    plotOutput("total_pi_pidistr")),
           tabPanel("Bootstrap Results",
                    div(style = "overflow-x: scroll", DTOutput("total_pi_boot"))))
  )
)




## Explore PI -----------------------------------------------------------------
ui_total_pi_map <- fluidRow(
  column(width = 12,
         h2("Exploring Predictions"),
         p(downloadButton(
             "total_boot_download", "Download full results as Excel file")),
         box(width = 3,
             h4("Subsets"),
             uiOutput("total_pi_subset_col"),
             uiOutput("total_pi_subset_group"),

             hr(),
             h4("Map options"),
             selectInput("total_pi_col", label = "Variable to map",
                         choices = c("observed_values", "fitted_values",
                                     "Cell.mean", "Cell.mode", "Cell.pred",
                                     "Cell.PIL", "Cell.PIU",
                                     "Cell.accuracy", "Residuals")),
             sliderInput("total_pi_bins",
                         label = "Number of colour-bins", value = 5,
                         min = 2, max = 10),
             bsButton("total_pi_reset", "Reset selection", style = "primary")),
         tabBox(width = 9,
                tabPanel(title = "Map",
                         girafeOutput("total_pi_map")),
                tabPanel(title = "Plot",
                         uiOutput("total_pi_plot_col"),
                         girafeOutput("total_pi_plot"))
         ),
         box(width = 12,
             h4("Data"),
             div(style = "overflow-x: scroll;margin-top:15px",
                 DTOutput("total_pi_data")))
  )
)


# Composition ------------------------------------------------------------

## Explore -------------------------------------------------------
ui_comp_explore <- fluidRow(
  column(width = 12,
         h2("Composition Exploration"),
         box(height = "100px", uiOutput("comp_explore_ui")),
         plotOutput("comp_explore_graph", width = "100%")
  )
)


## Models -----------------------------------------------------------
ui_comp_models <- fluidRow(
  column(width = 12,
         h2("Composition Model Fit"),
         column(width = 4,
                box(width = 12,
                    uiOutput("comp_model_id_ui"),
                    uiOutput("comp_model_var_ui"),
                    bsButton("comp_model_add", "Add", style = "primary")),
                box(width = 12,
                    h4("Error messages"),
                    uiOutput("comp_model_msgs"))),
         box(width = 8,
             h4("Current models"),
             tableOutput("comp_model_table"),
             uiOutput("comp_model_delete_ui"),
             hr(),
             h4("AIC Model Comparison"),
             div(style = "overflow-x: scroll", tableOutput("comp_model_aic")))
  )
)


## Prediction Intervals -------------------------------------------------
ui_comp_pi <- fluidRow(
  column(width = 12,
         h2("Composition Prediction Intervals"),

         column(width = 3,
                box(width = 12,
                    uiOutput("comp_pi_models_ui"),
                    uiOutput("comp_pi_average_ui"),
                    bsButton("comp_pi_calc", "Calculate PI",
                             style = "primary"),
                    hr(),
                    uiOutput("comp_pi_selected", style = "margin-top:5px")),
                box(width = 12,
                    title = "Issues and Options",
                    tableOutput("comp_pi_options"))),

         tabBox(width = 9,
                tabPanel(title = "Summary",
                         tableOutput("comp_pi_density")),
                tabPanel(title = "Bootstrap Results",
                         DTOutput("comp_pi_boot"))
         )
  )
)


## Summary ----------------------------------------------
ui_comp_sum <- fluidRow(
  column(width = 12,
         h2("Composition Summary"),
         p(downloadButton(
           "comp_boot_download", "Download full results as Excel file")),
         box(width = 3,
             h4("Subsets"),
             uiOutput("comp_pi_subset_col"),
             uiOutput("comp_pi_subset_group")),
         box(width = 9,
             h4("Predictions"),
             div(DTOutput("comp_pi_summary"), style = "min-height:100px"),
             h4("Summary - Subset"),
             tableOutput("comp_pi_density_subset")
         )
  )
)


# Docs -------------------
ui_docs <- fluidRow(
  column(width=12,
         h2("Documentation"),
         box(width = 12, class = "docs",
             includeMarkdown("docs.md")
         )
  )
)


# Combine -------------------------------------------------------------------

dashboardPage(
  dashboardHeader(title = paste("Moose Counter", ver[1])),
  ## Sidebar ----------------
  dashboardSidebar(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$script(src = "tips.js"),
    sidebarMenu(id = "menu",
      menuItem("Home", tabName = "home", icon=icon("home")),
      menuItem("Settings", tabName = "settings", icon=icon("cog")),
      menuItem("Data", tabName = "data", icon=icon("table")),
      menuItem("Total", tabName = "total", icon=icon("circle"),
               menuSubItem("Explore - Univariate", tabName = "total_univar"),
               menuSubItem("Explore - Multivariate", tabName = "total_multivar"),
               menuSubItem("Models", tabName = "total_models"),
               menuSubItem("Residuals", tabName = "total_residuals"),
               menuSubItem("Prediction Intervals", tabName = "total_pi"),
               menuSubItem("Explore Predictions", tabName = "total_pi_map")
      ),
      menuItem("Composition", tabName = "composition", icon = icon("chart-pie"),
               menuSubItem("Explore", tabName = "comp_explore"),
               menuSubItem("Models", tabName = "comp_models"),
               menuSubItem("Prediction Intervals", tabName = "comp_pi"),
               menuSubItem("Summary", tabName = "comp_sum")),
      menuItem("Documentation", tabName = "docs", icon=icon("book"))
    )
  ),

  ## Body -----------------------
  dashboardBody(
    tabItems(
      tabItem("home", ui_home),
      tabItem("docs", ui_docs),
      tabItem("data", ui_data),
      tabItem("settings", ui_settings),
      tabItem("total_univar", ui_total_univar),
      tabItem("total_multivar", ui_total_multivar),
      tabItem("total_models", ui_total_models),
      tabItem("total_residuals", ui_total_residuals),
      tabItem("total_pi", ui_total_pi),
      tabItem("total_pi_map", ui_total_pi_map),
      tabItem("comp_explore", ui_comp_explore),
      tabItem("comp_models", ui_comp_models),
      tabItem("comp_pi", ui_comp_pi),
      tabItem("comp_sum", ui_comp_sum),
      tabItem("docs", ui_docs)
    )
  )
)

