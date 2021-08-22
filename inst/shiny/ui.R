ui_home <- fluidRow(
  column(width=12,
    includeMarkdown("intro.md")
  )
)

ui_docs <- fluidRow(
  column(width=12,
    includeMarkdown("docs.md")
  )
)

ui_data <- fluidRow(
  column(width=12,
    h2("Data")
  )
)

ui_settings <- fluidRow(
  column(width=12,
    h2("Settings")
  )
)

ui_univar <- fluidRow(
  column(width=12,
    h2("Univar")
  )
)

ui_multivar <- fluidRow(
  column(width=12,
    h2("Multivar")
  )
)

ui_addmodel <- fluidRow(
  column(width=12,
    h2("Add model")
  )
)

ui_residuals <- fluidRow(
  column(width=12,
    h2("Residuals")
  )
)

ui_calculatepi <- fluidRow(
  column(width=12,
    h2("Calculate PI")
  )
)

ui_explorepi <- fluidRow(
  column(width=12,
    h2("Explore PI")
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
        menuSubItem("Resduals", tabName = "residuals"),
        menuSubItem("Calculate PI", tabName = "calculatepi"),
        menuSubItem("Explore PI", tabName = "explorepi")
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
      tabItem("calculatepi", ui_calculatepi),
      tabItem("explorepi", ui_explorepi)
    )
  )
)

