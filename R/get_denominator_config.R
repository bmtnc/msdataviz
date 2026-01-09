#' Get Denominator Configuration for DuPont Decomposition
#'
#' Returns configuration for a given denominator.
#'
#' @param denominator Short denominator name: "equity", "invested_capital", "total_assets"
#' @return List with: display_name, columns, is_calculated, return_label,
#'   effect_label, multiplier_label, title_suffix
#' @keywords internal
get_denominator_config <- function(denominator) {
  avpipeline::validate_character_scalar(denominator, allow_empty = FALSE, name = "denominator")

  configs <- list(
    equity = list(
      display_name = "Equity",
      columns = "totalShareholderEquity",
      is_calculated = FALSE,
      return_label = "ROE",
      effect_label = "Financial Leverage Effect",
      multiplier_label = "Equity Multiplier",
      title_suffix = "ROE Decomposition (DuPont)"
    ),
    invested_capital = list(
      display_name = "Invested Capital",
      columns = c("totalShareholderEquity", "shortLongTermDebtTotal", "capitalLeaseObligations"),
      is_calculated = TRUE,
      return_label = "ROIC",
      effect_label = "Capital Efficiency Effect",
      multiplier_label = "Assets / IC",
      title_suffix = "ROIC Decomposition"
    ),
    total_assets = list(
      display_name = "Total Assets",
      columns = "totalAssets",
      is_calculated = FALSE,
      return_label = "ROA",
      effect_label = "Asset Turnover Effect",
      multiplier_label = "Asset Multiplier",
      title_suffix = "ROA Decomposition"
    )
  )

  if (!denominator %in% names(configs)) {
    stop(
      "Unknown denominator: '", denominator, "'. ",
      "Available: ", paste(names(configs), collapse = ", ")
    )
  }

  configs[[denominator]]
}
