#' Get Metric Configuration for Price Decomposition
#'
#' Returns configuration for a given metric short name.
#'
#' @param metric Short metric name (e.g., "nopat", "ebitda", "fcf")
#' @return List with: display_name, ttm_columns, is_calculated
#' @keywords internal
get_metric_config <- function(metric) {
  avpipeline::validate_character_scalar(metric, allow_empty = FALSE, name = "metric")

  configs <- list(
    nopat = list(
      display_name = "NOPAT",
      ttm_columns = c("ebit_ttm", "depreciationAndAmortization_ttm", "depreciation_ttm"),
      is_calculated = TRUE
    ),
    revenue = list(
      display_name = "Revenue",
      ttm_columns = "totalRevenue_ttm",
      is_calculated = FALSE
    ),
    grossProfit = list(
      display_name = "Gross Profit",
      ttm_columns = "grossProfit_ttm",
      is_calculated = FALSE
    ),
    ebit = list(
      display_name = "EBIT",
      ttm_columns = "ebit_ttm",
      is_calculated = FALSE
    ),
    ebitda = list(
      display_name = "EBITDA",
      ttm_columns = "ebitda_ttm",
      is_calculated = FALSE
    ),
    fcf = list(
      display_name = "FCF",
      ttm_columns = "fcf_ttm",
      is_calculated = FALSE
    ),
    netIncome = list(
      display_name = "Net Income",
      ttm_columns = "netIncome_ttm",
      is_calculated = FALSE
    ),
    operatingCashflow = list(
      display_name = "Operating Cash Flow",
      ttm_columns = "operatingCashflow_ttm",
      is_calculated = FALSE
    ),
    bvps = list(
      display_name = "Book Value",
      ttm_columns = "totalShareholderEquity",
      is_calculated = FALSE
    )
  )

  if (!metric %in% names(configs)) {
    stop(
      "Unknown metric: '", metric, "'. ",
      "Available: ", paste(names(configs), collapse = ", ")
    )
  }

  configs[[metric]]
}
