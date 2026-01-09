#' Get DuPont Display Labels
#'
#' Returns display labels for DuPont decomposition based on numerator and denominator.
#' Handles special cases like gross profit which produces custom metrics (GROIC, GROE).
#'
#' @param numerator Numerator metric name (e.g., "nopat", "grossProfit")
#' @param denominator Denominator name (e.g., "equity", "invested_capital")
#' @return List with: return_label, roa_label, effect_label, multiplier_label, title_suffix, footnote
#' @keywords internal
get_dupont_labels <- function(numerator, denominator) {
  avpipeline::validate_character_scalar(numerator, allow_empty = FALSE, name = "numerator")
  avpipeline::validate_character_scalar(denominator, allow_empty = FALSE, name = "denominator")

  # Get base config from denominator
  denom_config <- get_denominator_config(denominator)

  # Default labels from denominator config
  labels <- list(
    return_label = denom_config$return_label,
    roa_label = "ROA",
    effect_label = denom_config$effect_label,
    multiplier_label = denom_config$multiplier_label,
    title_suffix = denom_config$title_suffix,
    footnote = NULL
  )

  # Override for gross profit numerator
 if (numerator == "grossProfit") {
    gross_footnote <- paste0(
      "Using Gross Return on ", denom_config$display_name,
      " (gross profit in lieu of net profit). ",
      "This represents the upper bound of what true ",
      denom_config$return_label, " could be."
    )

    labels$roa_label <- "GROA"

    if (denominator == "invested_capital") {
      labels$return_label <- "GROIC"
      labels$title_suffix <- "GROIC Decomposition"
      labels$footnote <- gross_footnote
    } else if (denominator == "equity") {
      labels$return_label <- "GROE"
      labels$title_suffix <- "GROE Decomposition"
      labels$footnote <- gross_footnote
    } else if (denominator == "total_assets") {
      labels$return_label <- "GROA"
      labels$title_suffix <- "GROA Decomposition"
      labels$footnote <- gross_footnote
    }
  }

  labels
}
