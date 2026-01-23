#' Build Caption for Elliptic Envelope Plot
#'
#' Constructs a multi-line caption with population counts and winsorization note.
#'
#' @param n_population Number of stocks in the background population
#' @param n_focus_group Number of stocks in the focus group
#' @param population_label Label for the population
#' @param focus_group_label Label for the focus group
#' @param winsorize_pct Winsorization percentage (0-1 scale)
#'
#' @return A character string with newline-separated caption lines
#' @export
#' @keywords internal
build_envelope_caption <- function(
    n_population,
    n_focus_group,
    population_label,
    focus_group_label,
    winsorize_pct
) {
  caption_parts <- c()

  if (!is.null(n_population)) {
    caption_parts <- c(
      caption_parts,
      paste0(to_display_case(population_label), " population: ", n_population)
    )
  }

  if (!is.null(n_focus_group)) {
    caption_parts <- c(
      caption_parts,
      paste0(to_display_case(focus_group_label), " population: ", n_focus_group)
    )
  }

  winsorize_pct_display <- winsorize_pct * 100
  caption_parts <- c(
    caption_parts,
    sprintf(
      "Data winsorized at %.0fth/%.0fth percentiles",
      winsorize_pct_display,
      100 - winsorize_pct_display
    )
  )

  paste(caption_parts, collapse = "\n")
}
