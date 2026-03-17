#' Launch the Method Comparison Shiny app
#'
#' This wrapper allows Posit Connect and local developers to run the app via
#' `methodCompare::run_app()`, which loads the packaged app directory under
#' `inst/app`.
#' @export
run_app <- function(...) {
  app_dir <- system.file("app", package = "methodCompare")
  if (app_dir == "") {
    stop("Could not find app directory. Was the package installed correctly?")
  }
  shiny::runApp(appDir = app_dir, ...)
}
