#' Get Report Profile Configuration
#'
#' Returns default configuration for a named profile.
#'
#' @param profile Profile name: "default", "financial", "asset_heavy", "early_stage"
#' @return Named list with: price_decomp_metric, price_decomp_numerator,
#'   kpi_metric, dupont_numerator, dupont_denominator
#' @export
#' @keywords internal
get_report_profile <- function(profile = "default") {
  avpipeline::validate_character_scalar(profile, allow_empty = FALSE, name = "profile")

  profiles <- list(
    default = list(
      price_decomp_metric = "nopat",
      price_decomp_numerator = "price",
      kpi_metric = "nopat",
      dupont_numerator = "nopat",
      dupont_denominator = "invested_capital"
    ),
    financial = list(
      price_decomp_metric = "bvps",
      price_decomp_numerator = "price",
      kpi_metric = "bvps",
      dupont_numerator = "netIncome",
      dupont_denominator = "equity"
    ),
    asset_heavy = list(
      price_decomp_metric = "bvps",
      price_decomp_numerator = "price",
      kpi_metric = "bvps",
      dupont_numerator = "nopat",
      dupont_denominator = "invested_capital"
    ),
    early_stage = list(
      price_decomp_metric = "grossProfit",
      price_decomp_numerator = "price",
      kpi_metric = "grossProfit",
      dupont_numerator = "grossProfit",
      dupont_denominator = "invested_capital"
    )
  )

  if (!profile %in% names(profiles)) {
    stop(
      "Unknown profile: '", profile, "'. ",
      "Available: ", paste(names(profiles), collapse = ", ")
    )
  }

  profiles[[profile]]
}
