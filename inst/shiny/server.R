server <- function(input, output, session) {

  # Settings -----------------------------------
  opts <- reactive({
    input$opts_response
    input$opts_method
    input$opts_b
    input$opts_alpha
    input$opts_wscale
    input$opts_sightability


    switch_response(input$opts_response)
    mc_options(method = input$opts_method)
    mc_options(B = input$opts_b)
    mc_options(alpha = as.numeric(input$opts_alpha))

    mc_options(wscale = input$opts_wscale)
    mc_options(sightability = input$opts_sightability)
    #mc_options(srv = "")
    #mc_options(MAXCELL = "")
    #mc_options(area_srv = "")

    # Grab options to print
    getOption("moose_options")
  })

  output$opts <- renderTable({
    data.frame(option = names(opts()),
               values = sapply(opts(),
                               FUN = function(x) paste(x, collapse = ", ")))
  })


  # Data ---------------------------------------

  survey_data <- reactive({
    req(input$survey_file)

    # ADD CHECKS

    read.csv(input$survey_file$datapath)
  })

  # Filtering --------------------------------------
  # Consider dynamically adding inputs when user clicks button
  output$filters <- renderUI({
    req(survey_data())

    lapply(var_filter,
           FUN = function(x) {
             if(x %in% names(survey_data())) {
               o <- unique(survey_data()[[x]])
               selectInput(paste0("filter_", x),
                           label = HTML(paste0(names(var_filter[var_filter == x]),
                                               " (<code>", x, "</code>)")),
                           choices = o, multiple = TRUE)
             }
           })
  })


  survey_sub <- reactive({

    s <- survey_data()
    for(x in var_filter) {
      n <- paste0("filter_", x)
      if(x %in% names(s) && !is.null(input[[n]])) s <- s[s[[x]] %in% input[[n]], ]
    }

    # Apply filtering
    mc_update_total(s)
  })


  output$survey_preview <- renderDT({
    datatable(survey_sub())
  })


  # Univariate Exploration ---------------------------
  output$uni_var <- renderUI(select_dep("uni_var",
                                        "Univariate variable to explore",
                                        survey_sub()))

  output$uni_graph <- renderPlot({
    req(input$uni_var, input$uni_dist)
    req(input$uni_var != "none")
    req(opts())

    mc_plot_univariate(input$uni_var, survey_sub(), input$uni_dist)
  }, res = 125)


  # Multivariate Exploration ---------------------
  output$multi_var <- renderUI(select_dep("multi_var",
                                          "Multivariate variables to explore",
                                          survey_sub(),
                                          multiple = TRUE))

  output$multi_graph <- renderPlot({
    req(input$multi_var)
    req(input$multi_var != "none")
    req(opts())

    mc_plot_multivariate(input$multi_var, survey_sub())
  }, res = 100)
}

