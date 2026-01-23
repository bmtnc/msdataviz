#' Convert String to snake_case
#'
#' Converts strings like "SPECIALTY CHEMICALS" or "Specialty Chemicals" to "specialty_chemicals".
#'
#' @param x Character vector
#' @return Character vector in snake_case
#' @export
#' @keywords internal
to_snake_case <- function(x) {
  x %>%
    tolower() %>%
    stringr::str_replace_all("\\s+", "_") %>%
    stringr::str_replace_all("[^a-z0-9_]", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_remove("^_|_$")
}
