#' Run Shiny App
#'
#' Run the Shiny app to estimate total Moose and composition.
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
#' @export
run_app <- function() {
    shiny::runApp(
        system.file("shiny", package="moosecounter"),
        display.mode = "normal")
}
