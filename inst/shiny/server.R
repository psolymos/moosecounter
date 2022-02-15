server <- function(input, output, session) {

  # Settings -----------------------------------
  opts <- reactive({
    req(input$opts_response,
      input$opts_method,
      input$opts_b,
      input$opts_alpha,
      input$opts_wscale,
      input$opts_sightability)


    switch_response(input$opts_response)
    mc_options(
      method = input$opts_method,
      B = input$opts_b,
      alpha = as.numeric(input$opts_alpha),
      wscale = input$opts_wscale,
      sightability = input$opts_sightability)
    #mc_options(srv = "")
    #mc_options(MAXCELL = "")
    #mc_options(area_srv = "")

    # Grab options to print
    getOption("moose_options")
  })

  output$opts <- renderTable({
    o <- opts()
    data.frame(option = unlist(opts_description[match(names(o),
                                               names(opts_description))]),
               name = names(o),
               values = sapply(o,
                               FUN = function(x) paste(x, collapse = ", ")))
  })

  observeEvent(input$opts_seed, set.seed(input$opts_seed))


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

    if(!is.null(input$survey_omit)) s <- select(s, -any_of(input$survey_omit))

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
    a <- model_aic()
    t(a[rev(seq_len(nrow(a))),])
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

  output$resid_summary <- renderPrint({
    req(length(models()) > 0, input$resid_model)
    validate_models(models())

    cat("Model:", input$resid_model, "\n")
    cat("Model type:", models()[[input$resid_model]][["dist"]],
        if (models()[[input$resid_model]][["weighted"]])
          "(weighted)" else "", "\n")
    summary(models()[[input$resid_model]][["model"]])
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

    tibble(Issues
           = if_else(length(pi$issues) == 0, "None",
                            as.character(length(pi$issues))),
           B = ncol(pi$boot_full),
           Method = pi()$opts$method,
           Response = if_else(pi()$opts$response == "total",
                              "MOOSE_TOTA",
                              "COW_TOTA"),
           Sightability = pi()$opts$sightability) %>%
      t() %>%
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

  # Bootstraps table
  output$pred_boot <- renderDT({
    PI <- mc_get_pred(pi()$pi)
    d <- data.frame(SU_ID=PI$data$SU_ID, PI$boot_full)
    datatable(d)
  })




  # Explore PI ----------------------------------------------------

  output$pred_data <- renderDT({
    validate(
      need(input$survey_file,
           "First select a data set in the \"Data\" tab") %then%
        need(length(models_list$m) > 0,
             "First create models in the \"Models\" tab") %then%
        need(!is.null(input$pred_models),
             "First create the predictions in the \"Prediction Intervals\" tab"))
    d <- mc_get_pred(pi()$pi)$data
    d[d$srv, c("Cell.mean", "Cell.mode", "Cell.pred", "Cell.PIL", "Cell.PIU",
      "Cell.accuracy")] <- NA
    v <- c("SU_ID", "observed_values", "fitted_values",
      "Cell.mean", "Cell.mode", "Cell.pred", "Cell.PIL", "Cell.PIU",
      "Cell.accuracy", "Residuals",
      "srv", "area_srv", "sort_id")
    datatable(d[,c(intersect(colnames(d), v), setdiff(colnames(d), v))]) %>%
      formatRound(columns = c("fitted_values", "Residuals", "Cell.accuracy", "Cell.PIL", "Cell.PIU"),
                  digits = 3)
  })

  # Setup table and table proxy
  pred_data_proxy <- dataTableProxy("pred_data")

  # Render map
  output$pred_map <- renderGirafe({
    req(pi())

    d <- mc_get_pred(pi()$pi)$data
    d[d$srv, c("Cell.mean", "Cell.mode", "Cell.pred",
               "Cell.PIL", "Cell.PIU", "Cell.accuracy")] <- NA
    d <- d %>%
      mutate(cell = 1:n(),
#             Cell.accuracy = if_else(Cell.accuracy == 0,
#                                     NA_real_,
#                                     Cell.accuracy),
             tooltip = paste0(
               "Cell = ", cell,
               if_else(is.na(observed_values),
                       paste0("<br>Cell Accuracy = ", round(Cell.accuracy, 3)),
                       paste0("<br>Observed = ", observed_values))))

    g <- ggplot(data = d,
                aes_string(x = "CENTRLON", y = "CENTRLAT",
                           fill = input$pred_col, data_id = "cell")) +
      geom_tile_interactive(aes(tooltip = tooltip))+
      coord_map() +
      scale_fill_binned(type = "viridis", n.breaks = input$pred_bins)

    girafe(ggobj = g,
           options = list(opts_selection(type = "multiple")))
  })


  # Select map cells when table rows selected
  observe({

    input$pred_data_rows_selected # Click on row
    input$pred_map_selected       # Click on map

    # Either way, highlights row selection
    isolate({
      session$sendCustomMessage(
        type = 'pred_map_set',
        message = as.character(input$pred_data_rows_selected))
    })
  })

  observeEvent(input$pred_reset, {
    pred_data_proxy %>% selectRows(NULL)
  })

  # PI/bootstrap download
  get_xlslist <- reactive({
    req(input$survey_file, pi())
    o <- mc_options()
    o <- append(o, c("random seed" = input$opts_seed))
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



  # Composition --------------------------------------------------------------

  ## Exploration ---------------------------
  output$comp_explore_ui <- renderUI({
    validate(need(input$survey_file,
                  "First select a data set in the \"Data\" tab"))

    select_explanatory("comp_explore_var",
                       "Variable to explore",
                       survey_sub())
  })

  output$comp_explore_graph <- renderPlot({
    req(input$comp_explore_var)
    req(input$comp_explore_var != "none")
    req(opts())

    # Check class sums
    mc_check_comp(survey_sub())

    mc_plot_comp(input$comp_explore_var, survey_sub())
  }, res = 125)


  ## Add Models -------------------------------------------------------------

  comp_models_list <- reactiveValues(m = list())

  output$comp_model_id_ui <- renderUI({
    validate(need(input$survey_file,
                  "First select a data set in the \"Data\" tab"))

    if(length(comp_models_list$m) == 0 || is.null(comp_models())) {
      val <- "A"
    } else {
      val <- LETTERS[!LETTERS %in% names(comp_models())][1]
    }

    textInput("comp_model_id", "Model ID", value = val)
  })

  output$comp_model_var_ui <- renderUI({
    select_explanatory("comp_model_var",
                       "Variables",
                       survey_sub(),
                       multiple = TRUE)
  })

  # Create reactive for using the models, not modifying, models
  comp_models <- reactive({
    req(length(comp_models_list$m) > 0)
    req(opts()) # To invalidate when settings change

    m <- comp_models_list$m
    m <- m[!map_lgl(m, is.null)]
    m[order(names(m))]

    # Record a change in comp_models()
    # isolate({
    #   if(is.null(input$pred_calc) || input$pred_calc > 0)
    #     updateButton(session, "pred_calc", style = "warning",
    #                  label = "Models have changed<br>(re-run)")
    # })

    # Run and add model and details
    # Evaluate directly to include args in the call itself, to prevent problems
    # with stats::update() later in the Prediction Interval steps.
    # idea from: https://stackoverflow.com/a/57528229/3362144
    map(m, ~ append(., c("model" = list(
      try(eval(rlang::expr(mc_fit_comp(x = survey_sub(),
                                       vars = !!.$var))),
          silent = TRUE)))))
  })

  output$comp_model_msgs <- renderUI({
    m <- map_chr(comp_models(), ~{
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


  observe({
    req(input$comp_model_id)
    comp_models_list$m[[input$comp_model_id]] <- list(var = input$comp_model_var)
  }) %>%
    bindEvent(input$comp_model_add)

  output$comp_model_table <- function() {
    imap_dfr(comp_models(),
             ~data.frame(Model = .y,
                         Variables = paste(.x$var, collapse = ", "))) %>%
      kable() %>%
      kable_styling() %>%
      row_spec(which(model_errors(comp_models())), background = "#f2dede")
  }

  # Dynamically create delete buttons for each model
  output$comp_model_delete_ui <- renderUI({
    req(length(comp_models()) > 0)

    m <- comp_models()[order(names(comp_models()))]

    imap(m, ~ bsButton(paste0("comp_delete_model_", .y),
                       label = .y,
                       icon = icon("times"),
                       style = if_else("try-error" %in% class(.x$model),
                                       "danger",
                                       "default")))
  })

  # Dynamically create observeEvents for each model delete button
  observe({
    req(length(comp_models()) > 0)

    isolate({
      map(names(comp_models()), ~ {
        observeEvent(input[[paste0("comp_delete_model_", .)]], {
          comp_models_list$m[[.]] <- NULL
        }, ignoreInit = TRUE)
      })
    })
  })

  comp_model_aic <- reactive({
    req(length(comp_models()) > 0)
    req(opts())
    validate_models(comp_models())

    map(comp_models(), "model") %>%
      mc_models_comp() %>%
      as.data.frame() %>%
      mutate(across(everything(), round, 2))
  })

  output$comp_model_aic <- renderTable({
    a <- comp_model_aic()
    t(a[rev(seq_len(nrow(a))),])
  }, rownames = TRUE)


  ## PI ----------------------------------------------------

  # UI elements
  output$comp_pi_models_ui <- renderUI({
    validate(
      need(input$survey_file,
           "First select a data set in the \"Data\" tab") %then%
        need(length(models_list$m) > 0,
             "First create Total Models in the Total > Models tab") %then%
        need(length(comp_models_list$m) > 0,
             "First create Composition Models in the Composition > Models tab"))

    tagList(
      selectInput("comp_pi_models1",
                  label = "Total model to use",
                  choices = names(models()), multiple = FALSE),
      selectInput("comp_pi_models2",
                  label = "Composition model to use",
                  choices = names(comp_models()), multiple = FALSE))
  })

  output$comp_pi_average_ui <- renderUI({
    req(input$comp_pi_models1, input$comp_pi_models2)

    if(length(input$comp_pi_models1) > 1 |
       length(input$comp_pi_models2) > 1)  {
      radioButtons("comp_pi_average", label = "With multiple models...",
                   choices = c("Use best model" = FALSE,
                               "Average over models" = TRUE),
                   selected = TRUE)
    }
  })

  comp_pi <- reactive({
    validate(need(
      !is.null(input$comp_pi_models1) & !is.null(input$comp_pi_models2),
      "Please choose your model(s)"))

    validate_models(models())
    validate_models(comp_models())

    updateButton(session, "comp_pi_calc", style = "primary", label = "Calculate PI")

    if(is.null(input$comp_pi_average)) {
      do_avg <- FALSE
    } else {
      do_avg <- as.logical(input$comp_pi_average)
    }

    list(
      comp_pi = mc_predict_comp(
        total_model_id = input$comp_pi_models1,
        comp_model_id = input$comp_pi_models2,
        model_list_total = map(models(), "model"),
        model_list_comp = map(comp_models(), "model"),
        x = survey_sub(),
        do_avg = do_avg),
      opts = opts())
  }) %>%
    bindEvent(input$comp_pi_calc)

  # Tables
  output$comp_pi_density <- function() {
    req(comp_pi())
    pred_density_moose_CPI(comp_pi()$comp_pi) %>%
      as.data.frame() %>%
      mutate(type = rownames(.)) %>%
      select(type, everything()) %>%
      kable(escape = FALSE, row.names = FALSE, align = "lrrr") %>%
      kable_styling(bootstrap_options = "condensed")
  }

  output$comp_pi_options <- function() {
    req(comp_pi())

    comp_pi <- comp_pi()$comp_pi

    tibble(Issues = if_else(length(comp_pi$issues) == 0, "None",
                            as.character(length(comp_pi$issues))),
           Bootstraps = ncol(comp_pi()$opts$B),
           alpha = comp_pi()$opts$alpha,
           Method = comp_pi()$opts$method,
           Response = if_else(comp_pi()$opts$response == "total",
                              "MOOSE_TOTA",
                              "COW_TOTA"),
           Composition = paste(comp_pi()$opts$composition, collapse = ", "),
           Sightability = comp_pi()$opts$sightability) %>%
      t() %>%
      kable() %>%
      kable_styling(bootstrap_options = "condensed")
  }

}
