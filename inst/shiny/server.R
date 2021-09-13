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



  # Add models -----------------------------------
  output$model_var_count <- renderUI(select_dep("model_var_count",
                                                "Count Variables",
                                                survey_sub(),
                                                multiple = TRUE))
  output$model_var_zero <- renderUI(select_dep("model_var_zero",
                                               "Zero Variables",
                                               survey_sub(),
                                               multiple = TRUE))
  models_list <- reactiveValues(m = list())

  # Create reactive for using the models, not modifying, models
  models <- reactive({
    req(opts, length(models_list$m) > 0) # Make sure models() invalidates if opts() change

    m <- models_list$m
    m <- m[!map_lgl(m, is.null)]
    m[order(names(m))]
  })

  output$model_id <- renderUI({
    if(length(models_list$m) == 0 || is.null(models())) {
      val <- "A"
    } else {
      val <- LETTERS[!LETTERS %in% names(models())][1]
    }

    textInput("model_id", "Model ID", value = val)
  })


  observeEvent(input$model_add, {
    req(input$model_dist, input$model_id, input$model_weighted)

    # Run and add model and details
    # Evaluate directly to include args in the call itself, to prevent problems
    # with stats::update() later in the Prediction Interval steps.
    # idea from: https://stackoverflow.com/a/57528229/3362144
    m <- eval(rlang::expr(mc_fit_total(x = survey_sub(),
                                       vars = !!input$model_var_count,
                                       zi_vars = !!input$model_var_zero,
                                       dist = !!input$model_dist,
                                       weighted = !!input$model_weighted))) %>%
      try(silent = TRUE)

    # FOR TESTING
    # if(input$model_id == "C") m <- try(stop("test stop"), silent = TRUE)

    # Message to user if error (because observer, must be explicit,
    # cannot rely on validate/need
    if("try-error" %in% class(m)) {
      msg <- paste("Model error: ", m[1])
    } else msg <- ""
    output$model_msgs <- renderText(msg)

    # Don't continue if error
    validate(need(!"try-error" %in% class(m), message = FALSE))

    models_list$m[[input$model_id]] <- list(
      dist = input$model_dist,
      weighted = as.logical(input$model_weighted),
      var_count = input$model_var_count,
      var_zero = input$model_var_zero,
      model = m)
  })

  output$model_table <- renderTable({
    imap_dfr(
      models(),
      ~data.frame(Model = .y,
                  `Count variables` = paste(.x$var_count, collapse = ", "),
                  `Zero variables` = paste(.x$var_zero, collapse = ", "),
                  Distribution = .x$dist,
                  Weighted = .x$weighted,
                  row.names = .y))
  })

  # Dynamically create delete buttons for each model
  output$model_delete <- renderUI({
    req(length(models()) > 0)

    m <- models()[order(names(models()))]

    imap(m, ~ actionButton(paste0("delete_model_", .y),
                           label = .y,
                           icon = icon("times")))
  })

  # Dynamically create observeEvents for each model delete button
  observe({
    req(length(models()) > 0)

    isolate({
      map(names(models()), ~ {
        observeEvent(input[[paste0("delete_model_", .)]], {
          models_list$m[[.]] <- NULL
        }, ignoreInit = TRUE)
      })
    })

  })


  output$model_aic <- renderTable({
    req(length(models()) > 0)
    req(opts())

    map(models(), "model") %>%
      mc_models_total(survey_sub()) %>%
      mutate(across(everything(), round, 2))
  }, rownames = TRUE)

  # Model residuals / diagnostics ------------------------------

  output$resid_models <- renderUI({
    req(length(models()) > 0)
    radioButtons("resid_model", label = "Model", inline = TRUE,
                 choices = sort(names(models())))
  })

  output$resid_plot <- renderPlot({
    req(length(models()) > 0, input$resid_model)

    map(models(), "model") %>%
      mc_plot_residuals(input$resid_model, ., survey_sub())
  })



  # Prediction Intervals ----------------------------------------------------

  output$pred_models <- renderUI({
    selectInput("pred_models",
                label = "Model(s) to calculate prediction intervals for",
                choices = names(models()), multiple = TRUE)
  })

  pi <- eventReactive(input$pred_calc, {
    req(length(models()) > 0,
        opts(),
        survey_sub(),
        input$pred_models, input$pred_average)

    mc_predict_total(
      model_id = input$pred_models,
      ml = map(models(), "model"),
      x = survey_sub(),
      do_boot = TRUE,
      do_avg = as.logical(input$pred_average)) # Options to adjust these?
  })

  # Tables
  output$pred_density <- renderTable({
    pred_density_moose_PI(pi())
  }, rownames = TRUE)

  # Plots
  output$pred_predpi <- renderPlot(mc_plot_predpi(pi()), res = 125)
  output$pred_pidistr <- renderPlot(mc_plot_pidistr(pi()), res = 85)
  output$pred_pidistrcell <- renderPlot(mc_plot_pidistrcell(pi()), res = 85)


}

