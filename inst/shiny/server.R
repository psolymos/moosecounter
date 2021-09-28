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

    try(d <- read.csv(input$survey_file$datapath), silent = TRUE)
    validate(need(!"try-error" %in% class(t),
                  "Error loading data. Is it a valid csv?"))

    d
  })

  output$survey_omit_ui <- renderUI({
    req(survey_data())

    vars <- survey_data() %>%
      select(-any_of(c(var_meta, var_resp))) %>%
      select(where(~is.integer(.) || is.character(.) || is.factor(.))) %>%
      names()

    # Check for missing levels in filtered data
    isolate({vars <- vars[missing_levels(survey_sub(), vars)]})

    selectInput("survey_omit",
                label = "Omit variables with too few surveyed levels",
                selected = vars, choices = vars, multiple = TRUE)
  })

  output$survey_factors_ui <- renderUI({
    req(survey_data())

    vars <- survey_data() %>%
      select(-any_of(c(var_meta, var_resp))) %>%
      select(where(is.integer)) %>%
      names()

    if(!is.null(input$survey_omit)) vars <- vars[!vars %in% input$survey_omit]

    isolate({
      if(!is.null(input$survey_factors)) {
        s <- vars[vars %in% input$survey_factors]
      } else s <- NULL
    })

    selectInput("survey_factors",
                label = "Convert integer to categorical",
                choices = vars, selected = s, multiple = TRUE)
  })


  # Filtering --------------------------------------
  output$filters_ui <- renderUI({
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

    # Convert selected integers to factors
    if(!is.null(input$survey_factors)) {
      s <- mutate(s, across(input$survey_factors, as.factor))
    }

    if(!is.null(input$survey_omit)) s <- select(s, -all_of(input$survey_omit))

    # Apply filtering
    mc_update_total(s)
  })


  output$survey_preview <- renderDT({
    datatable(survey_sub())
  })

  output$survey_response <- renderPrint({
    survey_sub() %>%
      select(any_of(var_resp)) %>%
      str()
  })

  output$survey_meta <- renderPrint({
    survey_sub() %>%
      select(any_of(var_meta)) %>%
      str()
  })

  output$survey_explanatory <- renderPrint({
    survey_sub() %>%
      select(-any_of(c(var_resp, var_meta))) %>%
      str()
  })


  # Univariate Exploration ---------------------------
  output$uni_var_ui <- renderUI({
    validate(need(input$survey_file,
                  "First select a data set in the \"Data\" tab"))

    select_explanatory("uni_var",
                       "Univariate variable to explore",
                       survey_sub())
  })

  output$uni_graph <- renderPlot({
    req(input$uni_var, input$uni_dist)
    req(input$uni_var != "none")
    req(opts())

    mc_plot_univariate(input$uni_var, survey_sub(), input$uni_dist)
  }, res = 125)


  # Multivariate Exploration ---------------------
  output$multi_var_ui <- renderUI({
    validate(need(input$survey_file,
                  "First select a data set in the \"Data\" tab"))
    select_explanatory("multi_var",
                       "Multivariate variables to explore",
                       survey_sub(),
                       multiple = TRUE)
  })

  output$multi_graph <- renderPlot({
    req(input$multi_var != "none", opts(), input$multi_alpha)

    mc_plot_multivariate(vars = input$multi_var, x = survey_sub(),
                         alpha = input$multi_alpha)
  }, res = 100)



  # Add models -----------------------------------

  output$model_id_ui <- renderUI({
    validate(need(input$survey_file,
                  "First select a data set in the \"Data\" tab"))

    if(length(models_list$m) == 0 || is.null(models())) {
      val <- "A"
    } else {
      val <- LETTERS[!LETTERS %in% names(models())][1]
    }

    textInput("model_id", "Model ID", value = val)
  })

  output$model_var_count_ui <- renderUI({
    validate(need(input$survey_file,
                  "First select a data set in the \"Data\" tab"))
    select_explanatory("model_var_count",
                       "Count Variables",
                       survey_sub(),
                       multiple = TRUE)
  })

  output$model_var_zero_ui <- renderUI(select_explanatory("model_var_zero",
                                                       "Zero Variables",
                                                       survey_sub(),
                                                       multiple = TRUE))

  models_list <- reactiveValues(m = list())

  # Create reactive for using the models, not modifying, models
  models <- reactive({
    req(length(models_list$m) > 0)
    req(opts()) # To invalidate when settings change

    m <- models_list$m
    m <- m[!map_lgl(m, is.null)]
    m[order(names(m))]

    # TESTING errors
    #if("C" %in% names(m)) m$C$dist <- "nope"

    # Record a change in models()
    isolate({
      if(is.null(input$pred_calc) || input$pred_calc > 0)
        updateButton(session, "pred_calc", style = "warning",
                     label = "Models have changed<br>(re-run)")
    })

    # Run and add model and details
    # Evaluate directly to include args in the call itself, to prevent problems
    # with stats::update() later in the Prediction Interval steps.
    # idea from: https://stackoverflow.com/a/57528229/3362144
    map(m, ~ append(., c("model" = list(
      try(eval(rlang::expr(mc_fit_total(x = survey_sub(),
                                        vars = !!.$var_count,
                                        zi_vars = !!.$var_zero,
                                        dist = !!.$dist,
                                        weighted = !!.$weighted))),
          silent = TRUE)))))
  })

  output$model_msgs <- renderUI({
    m <- map_chr(models(), ~{
      if("try-error" %in% class(.$model)) .$model[1] else "no problem"})
    m <- m[m != "no problem"]

    if(length(m) > 0) {
      msg <- imap(m, ~tagList(span("Problem with model ", strong(.y), ": ",
                                   class = "alert-danger"), br(),
                              .x, p())) %>%
        tagList()
    } else msg <- tagList()

    msg
  })


  observeEvent(input$model_add, {
    req(input$model_dist, input$model_id, input$model_weighted)

    models_list$m[[input$model_id]] <- list(
      dist = input$model_dist,
      weighted = as.logical(input$model_weighted),
      var_count = input$model_var_count,
      var_zero = input$model_var_zero)
  })

  output$model_table <- function() {
    imap_dfr(models(), ~{
      d <- data.frame(Model = .y,
                      `Count variables` = paste(.x$var_count, collapse = ", "),
                      `Zero variables` = paste(.x$var_zero, collapse = ", "),
                      Distribution = .x$dist,
                      Weighted = .x$weighted)
      if("try-error" %in% class(.x$model)) {
        d <- mutate(d, method = "MODEL PROBLEM", response = "MODEL PROBLEM")
      } else {
        d <- mutate(d,
                    method = .x$model$method,
                    response = names(.x$model$model)[1])
      }
      d
    }) %>%
      kable() %>%
      kable_styling() %>%
      row_spec(which(model_errors(models())), background = "#f2dede")
  }

  # Dynamically create delete buttons for each model
  output$model_delete_ui <- renderUI({
    req(length(models()) > 0)

    m <- models()[order(names(models()))]

    imap(m, ~ bsButton(paste0("delete_model_", .y),
                       label = .y,
                       icon = icon("times"),
                       style = if_else("try-error" %in% class(.x$model),
                                       "danger",
                                       "default")))
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

  model_aic <- reactive({
    req(length(models()) > 0)
    req(opts())
    validate_models(models())

    map(models(), "model") %>%
      mc_models_total(survey_sub()) %>%
      mutate(across(everything(), round, 2))
  })

  output$model_aic1 <- renderTable({
    model_aic()
  }, rownames = TRUE)

  output$model_aic2 <- renderTable({
    model_aic()
  }, rownames = TRUE)

  # Model residuals / diagnostics ------------------------------

  output$resid_models_ui <- renderUI({
    validate(need(length(models_list$m) > 0,
                  "First create models in the \"Models\" tab"))
    validate_models(models())

    radioButtons("resid_model", label = "Model", inline = TRUE,
                 choices = sort(names(models())))
  })

  output$resid_plot <- renderPlot({
    req(length(models()) > 0, input$resid_model)
    validate_models(models())

    map(models(), "model") %>%
      mc_plot_residuals(input$resid_model, ., survey_sub())
  })



  # Prediction Intervals ----------------------------------------------------

  # UI elements
  output$pred_models_ui <- renderUI({
    validate(need(input$survey_file,
                  "First select a data set in the \"Data\" tab") %then%
               need(length(models_list$m) > 0,
                    "First create models in the \"Models\" tab"))
    selectInput("pred_models",
                label = "Model(s) to use",
                choices = names(models()), multiple = TRUE)
  })

  output$pred_cell_ui <- renderUI({
    numericInput("pred_cell", label = "Cell to plot for predictions",
                 value = 1, min = 1, max = nrow(pi()$pi$data), step = 1)
  })

  pi <- eventReactive(input$pred_calc, {
    req(length(models()) > 0, input$pred_average)
    validate(need(input$pred_models, "Please choose your model(s)"))
    validate_models(models())

    updateButton(session, "pred_calc", style = "primary", label = "Calculate PI")

    list(pi = mc_predict_total(model_id = input$pred_models,
                               ml = map(models(), "model"),
                               x = survey_sub(),
                               do_boot = TRUE,
                               do_avg = as.logical(input$pred_average)),
         opts = opts())
  })

  # Tables
  output$pred_density <- function() {
    req(pi())
    pred_density_moose_PI(pi()$pi) %>%
      as.data.frame() %>%
      mutate(" " = c("Total Moose",
                     "Total Area (km<sup>2</sup>)",
                     "Density (Moose/km<sup>2</sup>)")) %>%
      select(` `, everything()) %>%
      kable(escape = FALSE, row.names = FALSE, align = "lrrrrr") %>%
      kable_styling(bootstrap_options = "condensed")
  }

  output$pred_options <- function() {
    req(pi())

    pi <- pi()$pi

    i <- paste0(pi$issues, collapse = "; ")

    tibble(Issues = if_else(i == "", "None", i),
           B = ncol(pi$boot_full),
           Method = pi()$opts$method,
           Response = if_else(pi()$opts$response == "total",
                              "MOOSE_TOTA",
                              "COW_TOTA")) %>%
      kable() %>%
      kable_styling(bootstrap_options = "condensed")
  }

  # Plots
  output$pred_predpi <- renderPlot(mc_plot_predpi(pi()$pi), res = 125)
  output$pred_pidistr <- renderPlot({
    req(input$pred_cell)
    validate(need(input$pred_cell <= nrow(pi()$pi$data) &
                    input$pred_cell > 0,
                  paste0("Out of cell range: There are only ",
                         nrow(pi()$pi$data),
                         " cells in the data")))
    op <- par(mfrow = c(1, 2))
    mc_plot_pidistr(pi()$pi)
    mc_plot_pidistr(pi()$pi, id = input$pred_cell)
    par(op)
  }, res = 100)




  # Explore PI ----------------------------------------------------

  output$pred_data <- renderDT({
    d <- mc_get_pred(pi()$pi)$data
    v <- c("observed_values", "fitted_values",
      "Cell.mean", "Cell.mode", "Cell.pred", "Cell.PIL", "Cell.PIU",
      "Cell.accuracy", "Residuals",
      "srv", "area_srv", "sort_id")
    datatable(d[,c(v, setdiff(colnames(d), v))])
  })
  output$pred_boot <- renderDT(datatable(mc_get_pred(pi()$pi)$boot_full))


  # PI/bootstrap download
  get_xlslist <- reactive({
    req(input$survey_file, pi())
    list(
      Info=data.frame(moosecounter=paste0(
        c("R package version: ", "Date of analysis: ", "File: "),
        c(ver, format(Sys.time(), "%Y-%m-%d"), input$survey_file$name))),
      Settings=data.frame(
        Option=names(o),
        Value=sapply(o, paste, sep="", collapse=", ")),
      Summary=pred_density_moose_PI(pi()$pi),
      Data=mc_get_pred(pi()$pi)$data,
      Boot=mc_get_pred(pi()$pi)$boot_full)
  })
  output$boot_download <- downloadHandler(
        filename = function() {
            paste0("Moose_Total_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
        },
        content = function(file) {
            write.xlsx(get_xlslist(), file=file, overwrite=TRUE)
        },
        contentType="application/octet-stream"
  )


  observeEvent(input$pred_data_rows_selected, {
    session$sendCustomMessage(type = 'pred_map_set', message = input$pred_data_rows_selected)
  })

  output$pred_map <- renderGirafe({
    req(pi())

    d <- mc_get_pred(pi()$pi)$data %>%
      mutate(cell = 1:n(),
             tooltip = paste0("Cell = ", cell,
                              "<br>Observed = ", observed_values))

    g <- ggplot(data = d, aes(x = CENTRLON, y = CENTRLAT,
                              fill = observed_values, data_id = cell)) +
      geom_tile_interactive(aes(tooltip = tooltip))+
      coord_map() +
      scale_fill_viridis_c()

    girafe(ggobj = g,
           options = list(opts_selection(type = "single")))
  })


}
