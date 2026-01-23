#' Convert snake_case to Display Case
#'
#' Converts strings like "specialty_chemicals" to "Specialty Chemicals".
#'
#' @param x Character vector
#' @return Character vector in Title Case
#' @export
#' @keywords internal
to_display_case <- function(x) {
  x %>%
    stringr::str_replace_all("_", " ") %>%
    tools::toTitleCase()
}
