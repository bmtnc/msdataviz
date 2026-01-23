#' Get Application Logger
#'
#' Returns the configured application logger.
#'
#' @return lgr Logger object
#' @export
get_app_logger <- function() {
  lgr::get_logger("msdataviz")
}
