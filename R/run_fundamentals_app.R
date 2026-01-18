#' Run Fundamentals Explorer Shiny App
#'
#' Launches an interactive Shiny app for exploring stock fundamentals.
#'
#' @param host Host address to bind to. Use "0.0.0.0" for Docker/external access.
#' @param port Port number for the Shiny app.
#' @param ... Additional arguments passed to shiny::runApp
#' @return NULL (opens Shiny app in browser)
#' @export
run_fundamentals_app <- function(host = "127.0.0.1", port = 3838, ...) {
  app_dir <- system.file("shiny", "fundamentals", package = "msdataviz")

  if (app_dir == "") {
    stop("Could not find Shiny app. Is msdataviz installed?")
  }

  shiny::runApp(app_dir, host = host, port = port, ...)
}
