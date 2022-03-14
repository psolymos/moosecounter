#' Run Shiny App
#'
#' Run the Shiny app to estimate total Moose and composition.
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
#' @keywords misc dynamic
#' @export
run_app <- function() {
    shiny::runApp(
        system.file("shiny", package="moosecounter"),
        display.mode = "normal")
}
