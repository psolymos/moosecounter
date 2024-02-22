server <- function(input, output, session) {

  # Settings -----------------------------------
  opts <- reactive({
    req(input$opts_response,
        input$opts_method,
        input$opts_b,
        input$opts_alpha,
        # input$opts_wscale,
        input$opts_sightability)

    switch_response(input$opts_response)
    mc_options(
      method = input$opts_method,
      B = input$opts_b,
      alpha = as.numeric(input$opts_alpha),
      # wscale = input$opts_wscale,
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

  # update subset variable list based on data
  var_subset_all <- reactive({
    req(survey_data())
    ds <- survey_data() %>%
      dplyr::select(-any_of(c(var_meta, var_resp)))
    # subset variables tend to have only 2 unique values
    uv <- sapply(ds, function(z) length(unique(z)))
    uv[is.na(uv)] <- 0
    sort(union(var_subset, colnames(ds)[uv == 2L]))
  })

  output$survey_omit_ui <- renderUI({
    req(survey_data())

    vars <- survey_data() %>%
      dplyr::select(-any_of(c(var_meta, var_resp))) %>%
      dplyr::select(where(~is.integer(.) || is.character(.) || is.factor(.))) %>%
      names()

    # Check for missing levels in filtered data
    isolate({vars <- vars[moosecounter:::missing_levels(survey_sub(), vars)]})

    selectInput("survey_omit",
                label = "Omit variables with too few surveyed levels",
                selected = character(0), choices = vars, multiple = TRUE)
  })

  output$survey_factors_ui <- renderUI({
    req(survey_data())

    vars <- survey_data() %>%
      dplyr::select(-any_of(c(var_meta, var_resp))) %>%
      dplyr::select(where(is.integer)) %>%
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
      s <- dplyr::mutate(s, across(input$survey_factors, as.factor))
    }

    if(!is.null(input$survey_omit)) s <- dplyr::select(s, -any_of(input$survey_omit))

    # Apply filtering
    mc_update_total(s)
  })


  output$survey_preview <- renderDT({
    datatable(survey_sub())
  })

  output$survey_response <- renderPrint({
    survey_sub() %>%
      dplyr::select(any_of(var_resp)) %>%
      str()
  })

  output$survey_meta <- renderPrint({
    survey_sub() %>%
      dplyr::select(any_of(var_meta)) %>%
      str()
  })

  output$survey_explanatory <- renderPrint({
    survey_sub() %>%
      dplyr::select(-any_of(c(var_resp, var_meta))) %>%
      str()
  })

  # Total Moose --------------------------------------------------------------
  ## Univariate Exploration --------------------------
  output$uni_var_ui <- renderUI({
    moosecounter:::validate_flow(input$survey_file)

    select_explanatory("uni_var",
                       "Univariate variable to explore",
                       survey_sub())
  })

  uni_graph <- reactive({
    req(input$uni_var, input$uni_dist, input$uni_var != "none", opts())

    p1 <- mc_plot_univariate(input$uni_var, survey_sub(), input$uni_dist,
                             base = FALSE, type = "density", interactive = TRUE)
    p2 <- mc_plot_univariate(input$uni_var, survey_sub(), input$uni_dist,
                             base = FALSE, type = "map", interactive = TRUE)
    p3 <- mc_plot_univariate(input$uni_var, survey_sub(), input$uni_dist,
                             base = FALSE, type = "fit", interactive = TRUE)
    p1 + p2 + p3
  })

  observe(shinyjs::toggleState("dl_uni_graph", is_ready(uni_graph())))
  output$uni_graph <- renderGirafe(moosecounter:::mc_ggiraph(uni_graph(), width = 15, height = 4))
  output$dl_uni_graph <- plot_download(uni_graph(), "total_uni_var.png",
                                       dims = c(15, 4))


  ## Multivariate Exploration ---------------------
  output$multi_var_ui <- renderUI({
    moosecounter:::validate_flow(input$survey_file)
    select_explanatory("multi_var",
                       "Multivariate variables to explore",
                       survey_sub(),
                       multiple = TRUE)
  })

  multi_graph <- reactive({
    req(input$multi_var != "none", opts(), input$multi_alpha)
    mc_plot_multivariate(vars = input$multi_var, x = survey_sub(),
                         alpha = input$multi_alpha)
    grDevices::recordPlot()
  })
  observe(shinyjs::toggleState("dl_multi_graph", is_ready(multi_graph())))
  output$multi_graph <- renderPlot(multi_graph(), res = 100)
  output$dl_multi_graph <- plot_download(multi_graph, "total_multi_var.png",
                                         dims = c(10, 5))


  ## Add models -----------------------------------

  output$total_model_id_ui <- renderUI({
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
                                        xv = !!.$xv,
                                        # weighted = !!.$weighted,
                                        # robust = !!.$robust,
                                        intercept = !!.$intercept))),
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
    req(input$total_model_dist, input$total_model_id, input$total_model_xv)
    # req(input$total_model_dist, input$total_model_id, input$total_model_weighted, input$total_model_robust)

    total_models_list$m[[input$total_model_id]] <- list(
      dist = input$total_model_dist,
      xv = input$total_model_xv,
      # robust = as.logical(input$total_model_robust),
      # weighted = as.logical(input$total_model_weighted),
      var_count = input$total_model_var_count,
      var_zero = input$total_model_var_zero,
      intercept = tolower(input$total_model_int))
  }) %>%
    bindEvent(input$total_model_add)

  # find total surveyed moose
  observed_total <- reactive({
    s <- survey_sub()
    s <- s[s$srv,]
    n <- nrow(s)
    N <- sum(s[[opts()$Ntot]])
    c(n=n, N=N)
  })

  output$total_model_table <- function() {
    moosecounter:::validate_flow(input$survey_file)
    tot <- observed_total()
    imap_dfr(total_models(), ~{
      d <- data.frame(Model = .y,
                      `Count variables` = paste(.x$var_count, collapse = ", "),
                      `Zero variables` = paste(.x$var_zero, collapse = ", "),
                      Distribution = .x$dist,
                      # Surveys = tot$n,
                      # Total = tot$N,
                      check.names = FALSE)
                      # Weighted = .x$weighted,
                      # Robust = .x$robust)
      d <- dplyr::mutate(d,
        Total = paste0(tot["N"], " (n = ", tot["n"], ")"))
      if("try-error" %in% class(.x$model)) {
        d <- dplyr::mutate(d, method = "MODEL PROBLEM", response = "MODEL PROBLEM")
      } else {
        d <- dplyr::mutate(d,
                    Method = .x$model$method,
                    Response = names(.x$model$model)[1])
      }
      d
    }) %>%
      kable() %>%
      kable_styling() %>%
      row_spec(which(moosecounter:::model_errors(total_models())), background = "#f2dede")
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
    moosecounter:::validate_model_errors(total_models())

    map(total_models(), "model") %>%
      mc_models_total(survey_sub()) %>%
      dplyr::mutate(across(everything(), ~round(.x, 2)))
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
    moosecounter:::validate_flow(input$survey_file, models = total_models_list$m)
    moosecounter:::validate_model_errors(total_models())

    radioButtons("total_resid_model", label = "Model", inline = TRUE,
                 choices = sort(names(total_models())))
  })

  output$total_resid_plot <- renderPlot({
    req(length(total_models()) > 0, input$total_resid_model)
    moosecounter:::validate_model_errors(total_models())

    map(total_models(), "model") %>%
      mc_plot_residuals(input$total_resid_model, ., survey_sub())
  })

  output$total_resid_summary <- renderPrint({
    req(length(total_models()) > 0, input$total_resid_model)
    moosecounter:::validate_model_errors(total_models())

    cat("Model:", input$total_resid_model, "\n")
    cat("Model type:", total_models()[[input$total_resid_model]][["dist"]],
        # if (total_models()[[input$total_resid_model]][["weighted"]]) "(weighted)" else "",
        "\n")
    summary(total_models()[[input$total_resid_model]][["model"]])
  })



  ## Prediction Intervals ----------------------------------------------------

  # UI elements
  output$total_pi_models_ui <- renderUI({
    selectInput("total_pi_models",
                label = "Model(s) to use",
                choices = names(total_models()), multiple = TRUE)
  })

  output$total_pi_cell_ui <- renderUI({
    numericInput("total_pi_cell", label = "Cell to plot for predictions",
                 value = 1, min = 1, max = nrow(total_pi()$pi$data), step = 1)
  })

  total_pi_done <- reactiveVal(FALSE)
  total_pi <- reactive({
    req(length(total_models()) > 0, input$total_pi_average)
    validate(need(input$total_pi_models, "Please choose your model(s)"))
    moosecounter:::validate_model_errors(total_models())

    updateButton(session, "total_pi_calc", style = "primary",
                 label = "Calculate PI")
    total_pi_done(TRUE)
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
    moosecounter:::validate_flow(input$survey_file, models = total_models_list$m)
    total_pi()$pi$total %>%
      as.data.frame() %>%
      dplyr::mutate(" " = c("Total Moose",
                     "Total Area (km<sup>2</sup>)",
                     "Density (Moose/km<sup>2</sup>)")) %>%
      dplyr::select(` `, everything()) %>%
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
  output$total_pi_pidistr_all <- renderPlot({
    req(input$total_pi_bins_all)
    mc_plot_pidistr(total_pi()$pi, breaks = input$total_pi_bins_all)
  }, res = 100)
  output$total_pi_pidistr_cell <- renderPlot({
    req(input$total_pi_cell, input$total_pi_bins_cell)
    validate(need(input$total_pi_cell <= nrow(total_pi()$pi$data) &
                    input$total_pi_cell > 0,
                  paste0("Out of cell range: There are only ",
                         nrow(total_pi()$pi$data),
                         " cells in the data")))
    mc_plot_pidistr(total_pi()$pi, id = input$total_pi_cell, breaks = input$total_pi_bins_cell)
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
                   choices = var_subset_all()[var_subset_all() %in% names(survey_sub())])
  })

  output$total_pi_subset_group <- renderUI({
    req(input$total_pi_subset_col)
    selectizeInput("total_pi_subset_group",
                   label = "Groups to include",
                   choices = unique(survey_sub()[[input$total_pi_subset_col]]),
                   selected = unique(survey_sub()[[input$total_pi_subset_col]]),
                   multiple = TRUE)
  })

  # NOTE: total_pi() contains `pi` AND `opts, in contrast, `total_pi_sub() is
  #  just `pi`
  total_pi_subset <- reactive({
    req(input$total_pi_subset_col, input$total_pi_subset_group)
    ss <- total_pi()$pi$data[[input$total_pi_subset_col]]
    ss <- ss %in% input$total_pi_subset_group

    mc_get_pred(total_pi()$pi, ss = ss)
  })


  output$total_pi_data <- renderDT({
    d <- total_pi_subset()$data
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
    moosecounter:::validate_flow(input$survey_file, models = total_models_list$m,
                  pi = total_pi(), pi_subset = total_pi_subset())

    d <- total_pi_subset()$data
    d[d$srv, c("Cell.mean", "Cell.mode", "Cell.pred",
               "Cell.PIL", "Cell.PIU", "Cell.accuracy")] <- NA

    input_col <- input$total_pi_col
    set <- c("observed_values", "fitted_values", "Residuals")

    d <- d %>%
      dplyr::mutate(
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
                aes(x = .data$CENTRLON, y = .data$CENTRLAT,
                    fill = .data[[input$total_pi_col]], data_id = .data$cell)) +
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
  total_xlslist <- reactive({
    req(input$survey_file, total_pi())
    moosecounter:::PI_xlslist(input$survey_file,
               pred = total_pi()$pi,
               summary = pred_density_moose_PI(total_pi()$pi),
               seed = input$opts_seed)
  })

  total_xlslist_subset <- reactive({
    req(input$survey_file, total_pi_subset(),
        input$total_pi_subset_group, input$total_pi_subset_col)

    s <- pred_density_moose_PI(total_pi_subset())
    rownames(s) <- c("Total_Moose", "Total_Area_km2", "Density_Moose_Per_km2")

    moosecounter:::PI_xlslist(input$survey_file,
               pred = total_pi_subset(),
               summary = s,
               seed = input$opts_seed,
               subset = paste0(input$total_pi_subset_col, ": ",
                               paste0(input$total_pi_subset_group, collapse = ", ")))
  })

  # Downloader - Full Data
  output$total_boot_download <- downloadHandler(
    filename = function() {
      paste0("Moose_Total_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
    },
    content = function(file) {
      write.xlsx(total_xlslist(), file = file, overwrite = TRUE)
    },
    contentType = "application/octet-stream"
  )

  # Downloader - Subset Data
  output$total_boot_download_subset <- downloadHandler(
    filename = function() {
      paste0("Moose_Total_subset_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
    },
    content = function(file) {
      write.xlsx(total_xlslist_subset(), file = file, overwrite = TRUE)
    },
    contentType = "application/octet-stream"
  )

  # Downloader - Enable once have PIs
  observe({
    req(total_pi())
    shinyjs::enable("total_boot_download")
  })

  # Downloader - If Subset same as full, disable subset button
  observe({
    req(survey_sub(), input$total_pi_subset_col, input$total_pi_subset_group, total_pi())

    choices <- unique(survey_sub()[[input$total_pi_subset_col]])
    selected <- input$total_pi_subset_group

    shinyjs::toggleState(id = "total_boot_download_subset",
                    condition = !all(choices %in% selected))
  })


  output$total_pi_plot_col <- renderUI({
    moosecounter:::validate_flow(input$survey_file, models = total_models_list$m,
                  pi = input$total_pi_models, pi_subset = total_pi_subset()$data)

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

    req(input$total_pi_subset_col)
    ss <- total_pi()$pi$data[[input$total_pi_subset_col]]
    ss <- ss %in% input$total_pi_subset_group

    mc_plot_predfit(input$total_pi_plot_col, total_pi()$pi, ss = ss, interactive = TRUE)
  })

  output$total_pi_density_selected <- reactive({
    moosecounter:::validate_flow(input$survey_file, models = total_models_list$m,
                  pi = input$total_pi_models, pi_subset = total_pi_subset()$data)

    req(input$total_pi_subset_col)
    ss <- total_pi()$pi$data[[input$total_pi_subset_col]]
    ss <- ss %in% input$total_pi_subset_group

    d <- total_pi_subset()$total

    d %>%
      as.data.frame() %>%
      dplyr::mutate(" " = c("Total Moose",
                     "Total Area (km<sup>2</sup>)",
                     "Density (Moose/km<sup>2</sup>)")) %>%
      dplyr::select(` `, everything()) %>%
      kable(escape = FALSE, row.names = FALSE, align = "lrrrrr", digits = 3) %>%
      kable_styling(bootstrap_options = "condensed")
  })



  # Composition of Moose ----------------------------------------------------

  ## Exploration ---------------------------
  output$comp_explore_ui <- renderUI({
    moosecounter:::validate_flow(input$survey_file)

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
    moosecounter:::validate_flow(input$survey_file)
    imap_dfr(comp_models(),
             ~data.frame(Model = .y,
                         Variables = paste(.x$var, collapse = ", "))) %>%
      kable() %>%
      kable_styling() %>%
      row_spec(which(moosecounter:::model_errors(comp_models())), background = "#f2dede")
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
    moosecounter:::validate_model_errors(comp_models())

    map(comp_models(), "model") %>%
      mc_models_comp() %>%
      as.data.frame() %>%
      dplyr::mutate(across(everything(), ~round(.x, 2)))
  })

  output$comp_model_aic <- renderTable({
    a <- comp_model_aic()
    t(a[rev(seq_len(nrow(a))),])
  }, rownames = TRUE)


  ## Comp model residuals / diagnostics ------------------------------

  output$comp_resid_models_ui <- renderUI({
    moosecounter:::validate_flow(input$survey_file, models_comp = comp_models_list$m)
    moosecounter:::validate_model_errors(comp_models())

    radioButtons("comp_resid_model", label = "Composition Model", inline = TRUE,
                 choices = sort(names(comp_models())))
  })

  output$comp_model_aic2 <- renderTable({
    a <- comp_model_aic()
    t(a[rev(seq_len(nrow(a))),])
  }, rownames = TRUE)

  output$comp_resid_summary <- renderPrint({
    req(length(comp_models()) > 0, input$comp_resid_model)
    moosecounter:::validate_model_errors(comp_models())

    cat("Composition Model:", input$comp_resid_model, "\n")
    VGAM::summaryvglm(comp_models()[[input$comp_resid_model]][["model"]])
  })


  ## Comp PI ----------------------------------------------------

  # UI elements
  output$comp_pi_models_ui <- renderUI({
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

    moosecounter:::validate_model_errors(total_models())
    moosecounter:::validate_model_errors(comp_models())

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
        do_avg = do_avg,
        PI = if (total_pi_done()) total_pi()$pi else NULL),
      opts = opts())
  }) %>%
    bindEvent(input$comp_pi_calc)

  # Tables
  output$comp_pi_density <- function() {
    moosecounter:::validate_flow(input$survey_file,
                  models = total_models_list$m, models_comp = comp_models_list$m)
    req(comp_pi())
    pred_density_moose_CPI(comp_pi()$pi) %>%
      as.data.frame() %>%
      dplyr::mutate(type = rownames(.)) %>%
      dplyr::select(type, everything()) %>%
      kable(escape = FALSE, row.names = FALSE, align = "lrrrr") %>%
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
                   choices = var_subset_all()[var_subset_all() %in% names(survey_sub())])
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
    req(input$comp_pi_subset_col, input$comp_pi_subset_group)
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
    moosecounter:::validate_flow(input$survey_file, models = total_models_list$m,
                  models_comp = comp_models_list$m, pi = comp_pi(),
                  pi_subset = comp_pi_subset())

    cpi <- data.frame(SU_ID = comp_pi_subset()$data$SU_ID,
                      comp_pi_subset()$cells)
    datatable(cpi)
  })

  # Tables
  output$comp_pi_density_subset <- function() {
    req(length(comp_pi_subset()) > 0)
    pred_density_moose_CPI(comp_pi_subset()) %>%
      as.data.frame() %>%
      dplyr::mutate(type = rownames(.)) %>%
      dplyr::select(type, everything()) %>%
      kable(escape = FALSE, row.names = FALSE, align = "lrrrr") %>%
      kable_styling(bootstrap_options = "condensed")
  }

  # PI/bootstrap download
  comp_xlslist <- reactive({
    req(comp_pi())
    moosecounter:::PI_xlslist(input$survey_file,
               pred = comp_pi()$pi,
               summary = pred_density_moose_CPI(comp_pi()$pi),
               seed = input$opts_seed)
  })

  comp_xlslist_subset <- reactive({
    req(comp_pi_subset())
    moosecounter:::PI_xlslist(input$survey_file,
               pred = comp_pi_subset(),
               summary = pred_density_moose_CPI(comp_pi_subset()),
               seed = input$opts_seed,
               subset = paste0(input$comp_pi_subset_col, ": ",
                               paste0(input$comp_pi_subset_group, collapse = ", ")))
  })

  # Downloader - Full Data
  output$comp_boot_download <- downloadHandler(
    filename = function() {
      paste0("Moose_Composition_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
    },
    content = function(file) {
      write.xlsx(comp_xlslist(), file = file, overwrite = TRUE)
    },
    contentType = "application/octet-stream"
  )

  # Downloader - Subset Data
  output$comp_boot_download_subset <- downloadHandler(
    filename = function() {
      paste0("Moose_Composition_subset_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
    },
    content = function(file) {
      write.xlsx(comp_xlslist_subset(), file = file, overwrite = TRUE)
    },
    contentType = "application/octet-stream"
  )

  # Downloader - Enable once have PIs
  observe({
    req(comp_pi())
    shinyjs::enable("comp_boot_download")
  })

  # Downloader - If Subset same as full, disable subset button
  observe({
    req(survey_sub(), input$comp_pi_subset_col, input$comp_pi_subset_group, comp_pi())

    choices <- unique(survey_sub()[[input$comp_pi_subset_col]])
    selected <- input$comp_pi_subset_group

    shinyjs::toggleState(id = "comp_boot_download_subset",
                         condition = !all(choices %in% selected))
  })
}



