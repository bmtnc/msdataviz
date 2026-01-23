#' Configure Application Logger
#'
#' Creates a configured lgr logger for the Shiny application.
#'
#' @param log_dir Directory for log files
#' @param console_threshold Log level threshold for console
#' @param file_threshold Log level threshold for file
#' @param retention_days Number of days to retain log files
#'
#' @return Configured lgr Logger object
#' @export
configure_app_logger <- function(
    log_dir = "~/.cache/msdataviz/logs",
    console_threshold = "info",
    file_threshold = "debug",
    retention_days = 30
) {
  log_dir <- normalizePath(log_dir, mustWork = FALSE)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  logger <- lgr::get_logger("msdataviz")
  logger$set_threshold("debug")
  logger$set_propagate(FALSE)  # Don't propagate to root logger

  # Remove existing appenders to avoid duplicates on reconfiguration
  existing_appenders <- names(logger$appenders)
  if ("console" %in% existing_appenders) logger$remove_appender("console")
  if ("file" %in% existing_appenders) logger$remove_appender("file")

  # Console appender
  logger$add_appender(
    lgr::AppenderConsole$new(threshold = console_threshold),
    name = "console"
  )

  # File appender with daily rotation
  log_file <- file.path(
    log_dir,
    sprintf("fundamentals_app_%s.log", format(Sys.Date(), "%Y-%m-%d"))
  )
  logger$add_appender(
    lgr::AppenderFile$new(file = log_file, threshold = file_threshold),
    name = "file"
  )

  # Clean old log files
  clean_old_logs(log_dir, retention_days)

  logger
}
