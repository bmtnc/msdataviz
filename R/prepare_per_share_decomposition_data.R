#' Prepare Per-Share Decomposition Data
#'
#' Prepares data for plot_share_count_decomposition for a given metric.
#'
#' @param ttm_data TTM data frame from artifacts
#' @param ticker Character string for the ticker symbol
#' @param metric_col Column name containing the metric values
#' @param start_date Start date for filtering
#' @param end_date End date for filtering (NULL for no upper bound)
#'
#' @return Data frame ready for calculate_share_count_decomposition, or NULL if insufficient data
#' @export
prepare_per_share_decomposition_data <- function(
    ttm_data,
    ticker,
    metric_col,
    start_date,
    end_date = NULL
) {
  result <- ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::select(
      date = fiscalDateEnding,
      metric = dplyr::all_of(metric_col),
      shares = commonStockSharesOutstanding
    ) %>%
    dplyr::filter(
      !is.na(metric),
      !is.na(shares),
      metric > 0,
      shares > 0,
      date >= start_date
    ) %>%
    dplyr::arrange(date)

  if (!is.null(end_date)) {
    result <- result %>%
      dplyr::filter(date <= end_date)
  }

  if (nrow(result) < 2) {
    return(NULL)
  }

  base_date <- min(result$date)
  calculate_share_count_decomposition(result, base_date = base_date)
}
