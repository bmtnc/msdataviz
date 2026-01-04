#' Calculate Fundamental Per Share Value
#'
#' Calculates per-share value for any supported metric.
#'
#' @param ttm_data Data frame with TTM columns and commonStockSharesOutstanding
#' @param metric_config List from get_metric_config()
#' @return Numeric vector of per-share values
#' @keywords internal
calculate_fundamental_per_share <- function(ttm_data, metric_config) {
  shares <- ttm_data$commonStockSharesOutstanding

  if (metric_config$is_calculated) {
    avpipeline:::calculate_nopat_per_share(
      ttm_data$ebit_ttm / shares,
      ttm_data$depreciationAndAmortization_ttm / shares,
      ttm_data$depreciation_ttm / shares
    )
  } else {
    ttm_col <- metric_config$ttm_columns
    ttm_data[[ttm_col]] / shares
  }
}
