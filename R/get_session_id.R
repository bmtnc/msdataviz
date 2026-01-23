#' Get Shiny Session ID
#'
#' Extracts a short identifier from Shiny session for logging.
#'
#' @param session Shiny session object
#'
#' @return Character string with session identifier
#' @export
get_session_id <- function(session) {
  if (is.null(session)) return("no-session")
  if (!is.null(session$token)) {
    return(substr(session$token, 1, 8))
  }
  "unknown"
}
