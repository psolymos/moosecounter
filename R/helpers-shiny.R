

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

validate_model_errors <- function(m) {
  validate(need(!any(model_errors(m)),
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
      isTruthy(try(pi, silent = TRUE)) && length(pi) > 0,
      "First create the predictions in the Prediction Intervals tab")
  } else pi <- NULL

  if(!missing(pi_subset)) {
    pi_subset <- shiny::need(
      isTruthy(try(pi_subset, silent = TRUE)) && length(pi_subset) > 0,
      "No predictions. Make sure at least one group subset is selected")
  } else pi_subset <- NULL

  shiny::validate(survey_file %then%
                    models %then%
                    models_comp %then%
                    pi %then%
                    pi_subset)
}
