#' Calculate Fundamental Total Value
#'
#' Calculates total value for any supported metric (not per share).
#'
#' @param ttm_data Data frame with TTM columns
#' @param metric_config List from get_metric_config()
#' @param tax_rate Tax rate for NOPAT calculation (default: 0.2375)
#' @return Numeric vector of total values
#' @keywords internal
calculate_fundamental_total <- function(ttm_data, metric_config, tax_rate = 0.2375) {
  if (metric_config$is_calculated) {
    # NOPAT = (EBIT + amortization) * (1 - tax_rate)
    ebit <- ttm_data$ebit_ttm
    dep_amort <- dplyr::coalesce(ttm_data$depreciationAndAmortization_ttm, 0)
    depreciation <- dplyr::coalesce(ttm_data$depreciation_ttm, 0)
    amortization <- dep_amort - depreciation

    dplyr::case_when(
      is.na(ebit) ~ NA_real_,
      TRUE ~ (ebit + pmax(amortization, 0)) * (1 - tax_rate)
    )
  } else {
    ttm_col <- metric_config$ttm_columns
    ttm_data[[ttm_col]]
  }
}
