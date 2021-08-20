dashboardPage(
  dashboardHeader(title = paste("Moose Counter", ver[1])),
  dashboardSidebar(
    tags$script(src = "tips.js"),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon=icon("home")),
      menuItem("Tab A", tabName = "tabA",
               startExpanded=FALSE, icon=icon("dice-one"),
        menuSubItem("One", tabName = "one"),
        menuSubItem("Two", tabName = "two")
      ),
      menuItem("Tab B", tabName = "tabB", icon=icon("dice-two"),
        menuSubItem("Three", tabName = "three"),
        menuSubItem("Founr", tabName = "founr")
      ),
      menuItem("Documentation", tabName = "docs", icon=icon("book"))
    )
  ),
  dashboardBody(
    tabItems(

      tabItem("home",
        fluidRow(
          column(width=12,
                 includeMarkdown("intro.md")
          ),
        )
      ),


      tabItem("docs",
        fluidRow(
          column(width=12,
                 includeMarkdown("docs.md")
          ),
        )
      ),


      tabItem("one",
        fluidRow(
          column(width=12,
            h2("One")
          )
        )
      )


    )
  )
)

