## note: the ui_* UI pieces can be stored & sourced as separate file
## or added as modules

# Home -------------------
ui_home <- fluidRow(
  column(width=12,
    includeMarkdown("intro.md")
  )
)

# Docs -------------------
ui_docs <- fluidRow(
  column(width=12,
    includeMarkdown("docs.md")
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
                          choices = c(0.01, 0.05, 0.1)),
             bsTooltip("opts_alpha", opts_tooltip$alpha)
           ),

           column(width = 6,
             # Maxcell is optional
             # sliderInput("opts_maxcell",
             #             label = "Maximum abundance in cells",
             #             min = 1, max = 100),
             # bsTooltip("opts_maxcell", opts_tooltip$maxcell)

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

# Univariate ----------------
ui_univar <- fluidRow(
  column(width = 12,
    h2("Univariate Exploration"),
    box(height = "100px", uiOutput("uni_var_ui")),
    box(height = "100px",
        radioButtons("uni_dist", "Distribution", inline = TRUE,
                     choices = c("P", "NB", "ZIP", "ZINB"))),
    plotOutput("uni_graph", width = "100%")
  )
)

# Multivariate -------------------
ui_multivar <- fluidRow(
  column(width = 12,
    h2("Multivariate Exploration"),
    box(width = 6, uiOutput("multi_var_ui")),
    box(width = 6, sliderInput("multi_alpha", label = "alpha",
                               value = 0.10,
                               min = 0.001, max = 0.999, step = 0.01)),
    box(width = 12, plotOutput("multi_graph"))
  )
)

# Add model -------------------
ui_addmodel <- fluidRow(
  column(width = 12,
    h2("Add model"),
    column(width = 4,
           box(width = 12,
               uiOutput("model_id_ui"),
               uiOutput("model_var_count_ui"),
               uiOutput("model_var_zero_ui"),
               radioButtons("model_dist", "Distribution",
                            choices = c("P", "NB", "ZIP", "ZINB"), inline = TRUE),
               radioButtons("model_weighted", NULL,
                            c("Non weighted" = FALSE, "Weighted" = TRUE),
                            inline = TRUE),
               bsButton("model_add", "Add", style = "primary")),
           box(width = 12,
               h4("Error messages"),
               uiOutput("model_msgs"))),
    box(width = 8,
        h4("Current models"),
        tableOutput("model_table"),
        uiOutput("model_delete_ui"),
        hr(),
        h4("AIC Model Comparison"),
        div(style = "overflow-x: scroll", tableOutput("model_aic1")))
  )
)

# Residuals -----------------
ui_residuals <- fluidRow(
  column(width = 12,
    h2("Residuals"),
    box(width = 12,
        h4("AIC Model Comparison"),
        div(style = "overflow-x: scroll", tableOutput("model_aic2")),
        uiOutput("resid_models_ui"),
        plotOutput("resid_plot"))
  )
)

# Prediction Intervals -----------------
ui_pi <- fluidRow(
  column(width=12,
    h2("Calculating Prediction Intervals"),
    box(width = 4, height = "200px",
        column(width = 6,
               uiOutput("pred_models_ui"),
               bsButton("pred_calc", "Calculate PI",
                        style = "primary")),
        column(width = 6, conditionalPanel(
          condition = "input.pred_models.length > 1",
          radioButtons("pred_average", label = "With multiple models...",
                       choices = c("Use best model" = FALSE,
                                   "Average over models" = TRUE),
                       selected = TRUE)),
          uiOutput("pred_cell_ui"))),

    box(width = 5, height = "200px",
        title = "Summary",
        tableOutput("pred_density")),

    box(width = 3, height = "200px",
        title = "Options",
        tableOutput("pred_options")),

    tabBox(width = 12, id = "pi_panel",
           tabPanel("Diagnostic Plots", plotOutput("pred_predpi")),
           tabPanel("Moose Predictions",
                    plotOutput("pred_pidistr")),
           tabPanel("Bootstrap Results",
                    div(style = "overflow-x: scroll", DTOutput("pred_boot"))))
  )
)




# Explore PI -----------------------------------------------------------------
ui_pi_map <- fluidRow(
  column(width=12,
         h2("Exploring Predictions"),
         p(downloadButton(
             "boot_download", "Download results as Excel file"))
  ),
  column(width=12,
         box(width = 6,
             h4("Data"),
             bsButton("pred_reset", "Reset selection", style = "primary"),
             div(style = "overflow-x: scroll;margin-top:15px", DTOutput("pred_data"))),
         box(width = 6,
             h4("Map"),
                 selectInput("pred_col", label = "Variable to map",
                             choices = c("observed_values", "fitted_values",
                                         "Cell.mean", "Cell.mode", "Cell.pred",
                                         "Cell.PIL", "Cell.PIU",
                                         "Cell.accuracy", "Residuals")),
             girafeOutput("pred_map"))
  )
)


# Combine -------------------------------------------------------------------

dashboardPage(
  dashboardHeader(title = paste("Moose Counter", ver[1])),
  dashboardSidebar(
    tags$script(src = "tips.js"),
    sidebarMenu(id = "menu",
      menuItem("Home", tabName = "home", icon=icon("home")),
      menuItem("Settings", tabName = "settings", icon=icon("cog")),
      menuItem("Data", tabName = "data", icon=icon("table")),
      menuItem("Explore", tabName = "explore",
               startExpanded=FALSE, icon=icon("chart-bar"),
        menuSubItem("Univariate", tabName = "univar"),
        menuSubItem("Multivariate", tabName = "multivar")
      ),
      menuItem("Total", tabName = "total", icon=icon("circle"),
        menuSubItem("Models", tabName = "addmodel"),
        menuSubItem("Residuals", tabName = "residuals"),
        menuSubItem("Prediction Intervals", tabName = "pi"),
        menuSubItem("Explore Predictions", tabName = "pi_map")
      ),
      menuItem("Documentation", tabName = "docs", icon=icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("home", ui_home),
      tabItem("docs", ui_docs),
      tabItem("data", ui_data),
      tabItem("settings", ui_settings),
      tabItem("univar", ui_univar),
      tabItem("multivar", ui_multivar),
      tabItem("addmodel", ui_addmodel),
      tabItem("residuals", ui_residuals),
      tabItem("pi", ui_pi),
      tabItem("pi_map", ui_pi_map)
    )
  )
)

