#' Run Shiny App
#'
#' @export
run_app <- function() {
    shiny::runApp(
        system.file("shiny", package="moosecounter"),
        display.mode = "normal")
}
