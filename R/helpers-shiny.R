

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

validate_model_errors <- function(m) {
  shiny::validate(shiny::need(!any(model_errors(m)),
                paste0("Some models have problems (see 'Models' tab), ",
                       "adjust settings or remove models")))
}

validate_flow <- function(survey_file, models, models_comp, pi, pi_subset) {

  survey_file <- shiny::need(
    survey_file,
    "First select a data set in the Data tab")

  if(!missing(models)) {
    models <- shiny::need(
      length(models) > 0,
      "First create Total Models in the Total > Models tab<br>")
  } else models <- NULL

  if(!missing(models_comp)) {
    models_comp <- shiny::need(
      length(models_comp) > 0,
      "First create Composition Models in the Composition > Models tab")
  } else models_comp <- NULL

  if(!missing(pi)) {
    pi <- shiny::need(
      shiny::isTruthy(try(pi, silent = TRUE)) && length(pi) > 0,
      "First create the predictions in the Prediction Intervals tab")
  } else pi <- NULL

  if(!missing(pi_subset)) {
    pi_subset <- shiny::need(
      shiny::isTruthy(try(pi_subset, silent = TRUE)) && length(pi_subset) > 0,
      "No predictions. Make sure at least one group subset is selected")
  } else pi_subset <- NULL

  shiny::validate(survey_file %then%
                    models %then%
                    models_comp %then%
                    pi %then%
                    pi_subset)
}

#' UI for plot download button
#'
#' @param id Id of the plot object
#'
#' @noRd
ui_plot_download <- function(id) {
  shiny::div(align = "right",
      shinyjs::disabled(
        shiny::downloadButton(paste0("dl_", id), label = NULL, title = "Download plot")
      )
  )
}

#' Download plot
#'
#' @param plot Reactive plot object
#' @param file_name file name to save as
#' @param dims Vector of plot width and plot height
#' @param dpi Resolution
#'
#' @noRd
plot_download <- function(plot, file_name, dims = c(8, 8), dpi = 400) {
  shiny::downloadHandler(
    filename = function() {
      file_name
    },
    content = function(file) {
      shiny::req(plot())
      id <- shiny::showNotification("Downloading plot...", duration = NULL, closeButton = FALSE)
      on.exit(shiny::removeNotification(id), add = TRUE)

      if(inherits(plot(), "ggplot")) {
        ggplot2::ggsave(file, plot(), device = "png",
                        width = dims[1], height = dims[2], dpi = dpi,
                        bg = "white") # Otherwise mc_plot_predfit() plots have transparent bg (?)
      } else {
        grDevices::png(filename = file, width = dims[1], height = dims[2], units = "in", res = dpi)
        grDevices::replayPlot(plot())
        grDevices::dev.off()
      }
    }
  )
}

is_ready <- function(reactive) {
  t <- try(reactive, silent = TRUE)
  !inherits(t, "try-error")
}

