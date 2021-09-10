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
                                      "SANN", "Brent")),
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
             bsTooltip("opts_sightability", opts_tooltip$sightability)
           ),
           box(width = 12,
               tableOutput("opts")))

         # Surveyed units?
         # Surveyed area?

  )
)

# Data -------------------
ui_data <- fluidRow(
  column(width = 12,
    h2("Data"),

    column(width = 4,
           fileInput("survey_file", "Choose Survey CSV File",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           uiOutput("filters")),

    column(width = 8,
           div(style = "overflow-x: scroll", DTOutput("survey_preview")))

  )
)

# Univariate ----------------
ui_univar <- fluidRow(
  column(width = 12,
    h2("Univariate Exploration"),
    box(uiOutput("uni_var")),
    box(radioButtons("uni_dist", "Distribution", choices = c("P", "NB", "ZIP", "ZINB"), inline = TRUE)),
    plotOutput("uni_graph", width = "100%")
  )
)

# Multivariate -------------------
ui_multivar <- fluidRow(
  column(width=12,
    h2("Multivariate Exploration"),
    box(uiOutput("multi_var")),
    plotOutput("multi_graph")
  )
)

# Add model -------------------
ui_addmodel <- fluidRow(
  column(width = 12,
    h2("Add model"),
    column(width = 4,
           uiOutput("model_id"),
           uiOutput("model_var_count"),
           uiOutput("model_var_zero"),
           radioButtons("model_dist", "Distribution",
                        choices = c("P", "NB", "ZIP", "ZINB"), inline = TRUE),
           radioButtons("model_weighted", NULL,
                        c("Non weighted" = FALSE, "Weighted" = TRUE),
                        inline = TRUE),
           actionButton("model_add", "Add")),
    column(width = 8,
           h4("Current models"),
           tableOutput("model_table"),
           uiOutput("model_delete"),
           hr(),
           h4("AIC Model Comparison"),
           tableOutput("model_aic"))
  )
)

# Residuals -----------------
ui_residuals <- fluidRow(
  column(width=12,
    h2("Residuals"),
    uiOutput("resid_models"),
    plotOutput("resid_plot")
  )
)

# Prediction Intervals -----------------
ui_pi <- fluidRow(
  column(width=12,
    h2("Prediction Intervals"),
    box(width = 4, height = "200px",
        uiOutput("pred_models"),
        conditionalPanel(
          condition = "input.pred_models.length > 1",
          radioButtons("pred_average", label = NULL, inline = TRUE,
                       choices = c("Use best model" = FALSE,
                                   "Average over models" = TRUE),
                       selected = TRUE)),
        actionButton("pred_calc", "Calculate Prediction Interval")),
    box(width = 8, height = "200px", title = "Prediction Interval Summary",
        tableOutput("pred_density")),
    tabBox(width = 12,
           tabPanel("Diagnostic Plots", plotOutput("pred_predpi")),
           tabPanel("Total Moose", plotOutput("pred_pidistr")),
           tabPanel("Moose in Cell", plotOutput("pred_pidistrcell")))
  )
)


dashboardPage(
  dashboardHeader(title = paste("Moose Counter", ver[1])),
  dashboardSidebar(
    tags$script(src = "tips.js"),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon=icon("home")),
      menuItem("Settings", tabName = "settings", icon=icon("cog")),
      menuItem("Data", tabName = "data", icon=icon("table")),
      menuItem("Explore", tabName = "explore",
               startExpanded=FALSE, icon=icon("chart-bar"),
        menuSubItem("Univariate", tabName = "univar"),
        menuSubItem("Multivariate", tabName = "multivar")
      ),
      menuItem("Total", tabName = "total", icon=icon("circle"),
        menuSubItem("Add model", tabName = "addmodel"),
        menuSubItem("Residuals", tabName = "residuals"),
        menuSubItem("Prediction Intervals", tabName = "pi")
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
      tabItem("explorepi", ui_explorepi)
    )
  )
)

