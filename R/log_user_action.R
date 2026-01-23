#' Log User Action
#'
#' Logs a user action with session context.
#'
#' @param action Character string describing the action
#' @param session Shiny session object
#' @param ... Additional named values for glue interpolation in action string
#'
#' @return Invisible NULL
#' @keywords internal
log_user_action <- function(action, session, ...) {
  logger <- get_app_logger()
  session_id <- get_session_id(session)
  params <- list(...)
  if (length(params) > 0) {
    action <- glue::glue(action, .envir = list2env(params))
  }
  msg <- paste0("[", session_id, "] ", action)

  logger$info(msg)

  invisible(NULL)
}
