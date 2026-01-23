#' Log Reactive Event
#'
#' Logs entry to a reactive expression or observer.
#'
#' @param name Character string naming the reactive
#' @param session Shiny session object
#' @param ... Additional context values (currently unused, reserved for future)
#'
#' @return Invisible NULL
#' @export
#' @keywords internal
log_reactive_event <- function(name, session, ...) {
  logger <- get_app_logger()
  session_id <- get_session_id(session)
  msg <- paste0("[", session_id, "] Reactive: ", name)

  logger$debug(msg)

  invisible(NULL)
}
