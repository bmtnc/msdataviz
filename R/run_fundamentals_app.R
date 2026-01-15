#' Run Fundamentals Explorer Shiny App
#'
#' Launches an interactive Shiny app for exploring stock fundamentals.
#'
#' @param ... Additional arguments passed to shiny::runApp
#' @return NULL (opens Shiny app in browser)
#' @export
run_fundamentals_app <- function(...) {
  app_dir <- system.file("shiny", "fundamentals", package = "msdataviz")

  if (app_dir == "") {
    stop("Could not find Shiny app. Is msdataviz installed?")
  }

  shiny::runApp(app_dir, ...)
}
