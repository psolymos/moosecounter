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

  observe(set.seed(input$opts_seed)) %>%
    bindEvent(input$opts_seed)


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

  # Total Moose --------------------------------------------------------------
  ## Univariate Exploration --------------------------
  output$uni_var_ui <- renderUI({
    validate(need(input$survey_file,
                  "First select a data set in the \"Data\" tab"))

    select_explanatory("uni_var",
                       "Univariate variable to explore",
                       survey_sub())
  })

  output$uni_graph1 <- renderGirafe({
    req(input$uni_var, input$uni_dist, input$uni_var != "none", opts())

    mc_plot_univariate(input$uni_var, survey_sub(), input$uni_dist,
                     base = FALSE, type = "density", interactive = TRUE)
  })

  output$uni_graph2 <- renderGirafe({
    req(input$uni_var, input$uni_dist, input$uni_var != "none", opts())

    mc_plot_univariate(input$uni_var, survey_sub(), input$uni_dist,
                     base = FALSE, type = "map", interactive = TRUE)
  })

  output$uni_graph3 <- renderGirafe({
    req(input$uni_var, input$uni_dist, input$uni_var != "none", opts())

    mc_plot_univariate(input$uni_var, survey_sub(), input$uni_dist,
                     base = FALSE, type = "fit", interactive = TRUE)
  })


  ## Multivariate Exploration ---------------------
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


  ## Add models -----------------------------------

  output$total_model_id_ui <- renderUI({
    validate(need(input$survey_file,
                  "First select a data set in the \"Data\" tab"))
    if(length(total_models_list$m) == 0 || is.null(total_models())) {
      val <- "A"
    } else {
      val <- LETTERS[!LETTERS %in% names(total_models())][1]
    }

    textInput("total_model_id", "Model ID", value = val)
  })

  output$total_model_var_count_ui <- renderUI({
    select_explanatory("total_model_var_count",
                       "Count Variables",
                       survey_sub(),
                       multiple = TRUE)
  })

  output$total_model_var_zero_ui <- renderUI(
    select_explanatory("total_model_var_zero",
                       "Zero Variables",
                       survey_sub(),
                       multiple = TRUE))

  total_models_list <- reactiveValues(m = list())

  # Create reactive for using the models, not modifying, models
  total_models <- reactive({
    req(length(total_models_list$m) > 0)
    req(opts()) # To invalidate when settings change

    m <- total_models_list$m
    m <- m[!map_lgl(m, is.null)]
    m[order(names(m))]

    # TESTING errors
    #if("C" %in% names(m)) m$C$dist <- "nope"

    # Record a change in total_models()
    isolate({
      if(is.null(input$total_pi_calc) || input$total_pi_calc > 0)
        updateButton(session, "total_pi_calc", style = "warning",
                     label = "Models have changed<br>(re-run)")
      # Also update comp pi
      if(is.null(input$comp_pi_calc) || input$comp_pi_calc > 0)
        updateButton(session, "comp_pi_calc", style = "warning",
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

  output$total_model_msgs <- renderUI({
    m <- map_chr(total_models(), ~{
      if("try-error" %in% class(.$model)) {
        .$model[1]
      } else if(.$model$converged == FALSE) {
        "did not converge"
      } else "no problem"
    })
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
    req(input$total_model_dist, input$total_model_id, input$total_model_weighted)

    total_models_list$m[[input$total_model_id]] <- list(
      dist = input$total_model_dist,
      weighted = as.logical(input$total_model_weighted),
      var_count = input$total_model_var_count,
      var_zero = input$total_model_var_zero)
  }) %>%
    bindEvent(input$total_model_add)

  output$total_model_table <- function() {
    imap_dfr(total_models(), ~{
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
      row_spec(which(model_errors(total_models())), background = "#f2dede")
  }

  # Dynamically create delete buttons for each model
  output$total_model_delete_ui <- renderUI({
    req(length(total_models()) > 0)

    m <- total_models()[order(names(total_models()))]

    imap(m, ~ bsButton(paste0("total_delete_model_", .y),
                       label = .y,
                       icon = icon("times"),
                       style = if_else("try-error" %in% class(.x$model),
                                       "danger",
                                       "default")))
  })

  # Dynamically create observe() %>% bindEvent()s for each model delete button
  observe({
    req(length(total_models()) > 0)

    isolate({
      map(names(total_models()), ~ {
        m <- .
        observe(total_models_list$m[[m]] <- NULL) %>%
          bindEvent(input[[paste0("total_delete_model_", m)]],
                    ignoreInit = TRUE)
      })
    })

  })

  total_model_aic <- reactive({
    req(length(total_models()) > 0)
    req(opts())
    validate_models(total_models())

    map(total_models(), "model") %>%
      mc_models_total(survey_sub()) %>%
      mutate(across(everything(), round, 2))
  })

  output$total_model_aic1 <- renderTable({
    a <- total_model_aic()
    t(a[rev(seq_len(nrow(a))),])
  }, rownames = TRUE)

  output$total_model_aic2 <- renderTable({
    total_model_aic()
  }, rownames = TRUE)

  ## Model residuals / diagnostics ------------------------------

  output$total_resid_models_ui <- renderUI({
    validate(need(length(total_models_list$m) > 0,
                  "First create models in the \"Models\" tab"))
    validate_models(total_models())

    radioButtons("total_resid_model", label = "Model", inline = TRUE,
                 choices = sort(names(total_models())))
  })

  output$total_resid_plot <- renderPlot({
    req(length(total_models()) > 0, input$total_resid_model)
    validate_models(total_models())

    map(total_models(), "model") %>%
      mc_plot_residuals(input$total_resid_model, ., survey_sub())
  })

  output$total_resid_summary <- renderPrint({
    req(length(total_models()) > 0, input$total_resid_model)
    validate_models(total_models())

    cat("Model:", input$total_resid_model, "\n")
    cat("Model type:", total_models()[[input$total_resid_model]][["dist"]],
        if (total_models()[[input$total_resid_model]][["weighted"]])
          "(weighted)" else "", "\n")
    summary(total_models()[[input$total_resid_model]][["model"]])
  })



  ## Prediction Intervals ----------------------------------------------------

  # UI elements
  output$total_pi_models_ui <- renderUI({
    validate(need(input$survey_file,
                  "First select a data set in the \"Data\" tab") %then%
               need(length(total_models_list$m) > 0,
                    "First create models in the \"Models\" tab"))
    selectInput("total_pi_models",
                label = "Model(s) to use",
                choices = names(total_models()), multiple = TRUE)
  })

  output$total_pi_cell_ui <- renderUI({
    numericInput("total_pi_cell", label = "Cell to plot for predictions",
                 value = 1, min = 1, max = nrow(total_pi()$pi$data), step = 1)
  })

  total_pi <- reactive({
    req(length(total_models()) > 0, input$total_pi_average)
    validate(need(input$total_pi_models, "Please choose your model(s)"))
    validate_models(total_models())

    updateButton(session, "total_pi_calc", style = "primary",
                 label = "Calculate PI")

    list(pi = mc_predict_total(model_id = input$total_pi_models,
                               ml = map(total_models(), "model"),
                               x = survey_sub(),
                               do_boot = TRUE,
                               do_avg = as.logical(input$total_pi_average)),
         opts = opts())
  }) %>%
    bindEvent(input$total_pi_calc)

  # Tables
  output$total_pi_density <- function() {
    total_pi()$pi$total %>%
      as.data.frame() %>%
      mutate(" " = c("Total Moose",
                     "Total Area (km<sup>2</sup>)",
                     "Density (Moose/km<sup>2</sup>)")) %>%
      select(` `, everything()) %>%
      kable(escape = FALSE, row.names = FALSE, align = "lrrrrr", digits = 3) %>%
      kable_styling(bootstrap_options = "condensed")
  }

  output$total_pi_options <- function() {
    req(total_pi())

    tibble(Issues
           = if_else(length(total_pi()$pi$issues) == 0, "None",
                     as.character(length(total_pi()$pi$issues))),
           B = ncol(total_pi()$pi$boot_full),
           Method = total_pi()$opts$method,
           Response = if_else(total_pi()$opts$response == "total",
                              "MOOSE_TOTA",
                              "COW_TOTA"),
           Sightability = total_pi()$opts$sightability) %>%
      t() %>%
      kable() %>%
      kable_styling(bootstrap_options = "condensed")
  }

  # Note showing what models presented
  output$total_pi_selected <- renderText({
    m <- total_pi()$pi$model_id
    paste0(
      "<strong>Showing results of model(s):</strong> ", paste0(m, collapse = ", "),
      if_else(length(m) > 1,
              paste0(" (", dplyr::if_else(total_pi()$pi$do_avg,
                                          "Averaged", "Best model"),
                     ")"),
              ""))
  })


  # Plots
  output$total_pi_predpi <- renderPlot(mc_plot_predpi(total_pi()$pi), res = 125)
  output$total_pi_pidistr <- renderPlot({
    req(input$total_pi_cell)
    validate(need(input$total_pi_cell <= nrow(total_pi()$pi$data) &
                    input$total_pi_cell > 0,
                  paste0("Out of cell range: There are only ",
                         nrow(total_pi()$pi$data),
                         " cells in the data")))
    op <- par(mfrow = c(1, 2))
    mc_plot_pidistr(total_pi()$pi)
    mc_plot_pidistr(total_pi()$pi, id = input$total_pi_cell)
    par(op)
  }, res = 100)

  # Bootstraps table
  output$total_pi_boot <- renderDT({
    PI <- mc_get_pred(total_pi()$pi)
    d <- data.frame(SU_ID = PI$data$SU_ID, PI$boot_full)
    datatable(d)
  })




  ## Explore PI ----------------------------------------------------
  output$total_pi_subset_col <- renderUI({
    req(survey_sub())
    selectizeInput("total_pi_subset_col",
                   label = "Column to subset by",
                   choices = var_subset[var_subset %in% names(survey_sub())])
  })

  output$total_pi_subset_group <- renderUI({
    req(input$total_pi_subset_col)
    selectizeInput("total_pi_subset_group",
                   label = "Groups to include",
                   choices = unique(survey_sub()[[input$total_pi_subset_col]]),
                   selected = unique(survey_sub()[[input$total_pi_subset_col]]),
                   multiple = TRUE)
  })

  total_pi_subset <- reactive({
    req(input$total_pi_subset_col)
    ss <- total_pi()$pi$data[[input$total_pi_subset_col]]
    ss <- ss %in% input$total_pi_subset_group

    mc_get_pred(total_pi()$pi, ss = ss)$data
  })


  output$total_pi_data <- renderDT({
    d <- total_pi_subset()
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
  total_pi_data_proxy <- dataTableProxy("total_pi_data")

  # Render map
  output$total_pi_map <- renderGirafe({
    validate(
      need(input$survey_file,
           "First select a data set in the \"Data\" tab") %then%
        need(length(total_models_list$m) > 0,
             "First create models in the \"Models\" tab") %then%
        need(!is.null(input$total_pi_models),
             "First create the predictions in the \"Prediction Intervals\" tab") %then%
        need(nrow(total_pi_subset()) > 0,
             "No predictions. Make sure at least one group subset is selected"))

    d <- total_pi_subset()
    d[d$srv, c("Cell.mean", "Cell.mode", "Cell.pred",
               "Cell.PIL", "Cell.PIU", "Cell.accuracy")] <- NA

    input_col <- input$total_pi_col
    set <- c("observed_values", "fitted_values", "Residuals")

    d <- d %>%
      mutate(
        Residuals = round(Residuals, 3),
        cell = 1:n(),
        data_cell = !is.na(observed_values),
        acc = paste0("<br>Cell Accuracy = ", round(Cell.accuracy, 3)),
        obs = paste0("<br>Observed = ", observed_values),
        col = paste0("<br>", input_col, " = ", .data[[input_col]]),
        id = paste0("SU_ID = ", cell),
        tooltip =
          case_when(
            input_col == "observed_values" & data_cell ~ paste0(id, obs),
            input_col %in% set & data_cell ~ paste0(id, obs, col),
            input_col %in% set & !data_cell ~ paste0(id, acc),
            input_col == "Cell.accuracy" & !data_cell ~ paste0(id, acc),
            !data_cell ~ paste0(id, acc, col),
            data_cell ~ paste0(id, obs)
          ))

    validate(need(
      length(unique(na.omit(d[, input$total_pi_col]))) > 1,
      paste0("Cannot plot. No variability in ", input$total_pi_col)))

    g <- ggplot(data = d,
                aes_string(x = "CENTRLON", y = "CENTRLAT",
                           fill = input$total_pi_col, data_id = "cell")) +
      geom_tile_interactive(aes(tooltip = tooltip))+
      coord_map() +
      scale_fill_binned(type = "viridis", n.breaks = input$total_pi_bins)

    girafe(ggobj = g,
           options = list(opts_selection(type = "multiple")),
           width_svg = 8, height_svg = 7)
  })


  # Select map cells when table rows selected
  observe({

    input$total_pi_data_rows_selected # Click on row
    input$total_pi_map_selected       # Click on map

    # Either way, highlights row selection
    isolate({
      session$sendCustomMessage(
        type = 'total_pi_map_set',
        message = as.character(input$total_pi_data_rows_selected))
    })
  })

  observe(total_pi_data_proxy %>% selectRows(NULL)) %>%
    bindEvent(input$total_pi_reset)

  # PI/bootstrap download
  get_xlslist <- reactive({
    req(input$survey_file, total_pi())
    o <- mc_options()
    o <- append(o, c("random seed" = input$opts_seed))
    list(
      Info=data.frame(moosecounter=paste0(
        c("R package version: ", "Date of analysis: ", "File: "),
        c(ver, format(Sys.time(), "%Y-%m-%d"), input$survey_file$name))),
      Settings=data.frame(
        Option=names(o),
        Value=sapply(o, paste, sep="", collapse=", ")),
      Summary=pred_density_moose_PI(total_pi()$pi),
      Data=mc_get_pred(total_pi()$pi)$data,
      Boot=mc_get_pred(total_pi()$pi)$boot_full)
  })

  output$total_boot_download <- downloadHandler(
        filename = function() {
            paste0("Moose_Total_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
        },
        content = function(file) {
            write.xlsx(get_xlslist(), file=file, overwrite=TRUE)
        },
        contentType="application/octet-stream"
  )


  output$total_pi_plot_col <- renderUI({
    validate(
      need(input$survey_file,
           "First select a data set in the \"Data\" tab") %then%
        need(length(total_models_list$m) > 0,
             "First create models in the \"Models\" tab") %then%
        need(!is.null(input$total_pi_models),
             "First create the predictions in the \"Prediction Intervals\" tab") %then%
        need(nrow(total_pi_subset()) > 0,
             "No predictions. Make sure at least one group subset is selected"))

    m <- unique(total_pi()$pi$model_select_id)

    vars <- map(total_models_list$m[m],
                ~c(.[["var_count"]], .[["var_zero"]])) %>%
      unlist() %>%
      unique()

    if(is.null(vars)) vars <- "No variables"

    selectInput("total_pi_plot_col", label = "Explanatory Variable",
                choices = vars)
  })

  output$total_pi_plot <- renderGirafe({
    req(input$total_pi_plot_col,
        input$total_pi_plot_col != "No variables")

    mc_plot_predfit(input$total_pi_plot_col, total_pi()$pi, interactive = TRUE)
  })




  # Composition of Moose ----------------------------------------------------

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
    isolate({
      if(is.null(input$comp_pi_calc) || input$comp_pi_calc > 0)
        updateButton(session, "comp_pi_calc", style = "warning",
                     label = "Models have changed<br>(re-run)")
    })

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

  # Dynamically create observe() %>% bindEvent()s for each model delete button
  observe({
    req(length(comp_models()) > 0)

    isolate({
      map(names(comp_models()), ~ {
        m <- .
        observe(comp_models_list$m[[m]] <- NULL) %>%
          bindEvent(input[[paste0("comp_delete_model_", m)]],
                    ignoreInit = TRUE)
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
        need(length(total_models_list$m) > 0,
             "First create Total Models in the Total > Models tab") %then%
        need(length(comp_models_list$m) > 0,
             "First create Composition Models in the Composition > Models tab"))

    tagList(
      selectInput("comp_pi_models1",
                  label = "Total model(s) to use",
                  choices = names(total_models()), multiple = TRUE),
      selectInput("comp_pi_models2",
                  label = "Composition model to use",
                  choices = names(comp_models()), multiple = FALSE))
  })

  output$comp_pi_average_ui <- renderUI({
    req(input$comp_pi_models1, input$comp_pi_models2)

    if(length(input$comp_pi_models1) > 1)  {
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

    validate_models(total_models())
    validate_models(comp_models())

    updateButton(session, "comp_pi_calc", style = "primary",
                 label = "Calculate PI")

    if(is.null(input$comp_pi_average)) {
      do_avg <- FALSE
    } else {
      do_avg <- as.logical(input$comp_pi_average)
    }

    list(
      pi = mc_predict_comp(
        total_model_id = input$comp_pi_models1,
        comp_model_id = input$comp_pi_models2,
        model_list_total = map(total_models(), "model"),
        model_list_comp = map(comp_models(), "model"),
        x = survey_sub(),
        do_avg = do_avg),
      opts = opts())
  }) %>%
    bindEvent(input$comp_pi_calc)

  # Tables
  output$comp_pi_density <- function() {
    req(comp_pi())
    pred_density_moose_CPI(comp_pi()$pi) %>%
      as.data.frame() %>%
      mutate(type = rownames(.)) %>%
      select(type, everything()) %>%
      kable(escape = FALSE, row.names = FALSE, align = "lrrr") %>%
      kable_styling(bootstrap_options = "condensed")
  }

  output$comp_pi_options <- function() {
    req(comp_pi())

    comp_pi <- comp_pi()$pi

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

  # Bootstraps table
  output$comp_pi_boot <- renderDT({
    data.frame(SU_ID = comp_pi()$pi$data$SU_ID,
               comp_pi()$pi$boot_full) %>%
      datatable()
  })

  # Note showing what models presented
  output$comp_pi_selected <- renderText({

    m_t <- comp_pi()$pi$total_model_id
    m_c <- comp_pi()$pi$comp_model_id

    paste0(
      "<strong>Showing results of</strong><br><strong>Total model(s):</strong> ",
      paste0(m_t, collapse = ", "),
      if_else(length(m_t) > 1,
              paste0(" (", dplyr::if_else(comp_pi()$pi$do_avg,
                                          "Averaged", "Best model"),
                     ")"),
              ""),
      "<br><strong>Composition model:</strong> ", m_c)
  })



  ## Summary --------------------------------------------------
  output$comp_pi_subset_col <- renderUI({
    req(survey_sub())
    selectizeInput("comp_pi_subset_col",
                   label = "Column to subset by",
                   choices = var_subset[var_subset %in% names(survey_sub())])
  })

  output$comp_pi_subset_group <- renderUI({
    req(input$comp_pi_subset_col)
    selectizeInput("comp_pi_subset_group",
                   label = "Groups to include",
                   choices = unique(survey_sub()[[input$comp_pi_subset_col]]),
                   selected = unique(survey_sub()[[input$comp_pi_subset_col]]),
                   multiple = TRUE)
  })

  comp_pi_subset <- reactive({
    req(input$comp_pi_subset_col)
    ss <- comp_pi()$pi$data[[input$comp_pi_subset_col]]
    ss <- ss %in% input$comp_pi_subset_group

    # Catch no valid subsets
    if(any(ss)) {
      cpi <- subset_CPI_data(comp_pi()$pi, ss = ss)
    } else {
      cpi <- list()
    }
    cpi
  })

  output$comp_pi_summary <- renderDT({
    validate(
      need(input$survey_file,
           "First select a data set in the \"Data\" tab") %then%
        need(length(comp_models_list$m) > 0,
             "First create models in the \"Models\" tab") %then%
        need(input$comp_pi_calc > 0,
             "First create the predictions in the \"Prediction Intervals\" tab") %then%
        need(length(comp_pi_subset()) > 0,
             "No predictions. Make sure at least one group subset is selected"))

    cpi <- data.frame(SU_ID = comp_pi_subset()$data$SU_ID,
                      comp_pi_subset()$cells)
    datatable(cpi)
  })

  # Tables
  output$comp_pi_density_subset <- function() {
    req(length(comp_pi_subset()) > 0)
    pred_density_moose_CPI(comp_pi_subset()) %>%
      as.data.frame() %>%
      mutate(type = rownames(.)) %>%
      select(type, everything()) %>%
      kable(escape = FALSE, row.names = FALSE, align = "lrrr") %>%
      kable_styling(bootstrap_options = "condensed")
  }

  # Download summary
  # PI/bootstrap download
  comp_summary_xlsx <- reactive({
    req(input$survey_file, comp_pi())
    o <- mc_options()
    o <- append(o, c("random seed" = input$opts_seed))

    list(
      Info = data.frame(moosecounter = paste0(
        c("R package version: ", "Date of analysis: ", "File: "),
        c(ver, format(Sys.time(), "%Y-%m-%d"), input$survey_file$name))),
      Settings = data.frame(
        Option = names(o),
        Value = sapply(o, paste, sep="", collapse=", ")),
      Summary = pred_density_moose_CPI(comp_pi()$pi),
      Data = comp_pi()$pi$data,
      Boot = comp_pi()$pi$boot_full)
  })

  output$comp_boot_download <- downloadHandler(
    filename = function() {
      paste0("Moose_Composition_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
    },
    content = function(file) {
      write.xlsx(comp_summary_xlsx(), file = file, overwrite = TRUE)
    },
    contentType = "application/octet-stream"
  )

}



