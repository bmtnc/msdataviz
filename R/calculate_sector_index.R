#' Calculate Equal-Weighted Sector Index
#'
#' Builds a sector index from daily price data using equal-weighted average returns.
#'
#' @param price_data Data frame with columns: ticker, date, adjusted_close
#' @param sector_tickers Character vector of tickers in the sector
#' @param start_date Optional start date for filtering (default: NULL)
#' @return Data frame with columns: date, sector_index, sector_cumulative_return, sector_drawdown
#' @export
calculate_sector_index <- function(price_data, sector_tickers, start_date = NULL) {
  avpipeline::validate_df_cols(price_data, c("ticker", "date", "adjusted_close"))
  avpipeline::validate_non_empty(sector_tickers, "sector_tickers")

  # Filter to sector tickers and date range

  sector_prices <- price_data %>%
    dplyr::filter(ticker %in% sector_tickers)

  if (!is.null(start_date)) {
    sector_prices <- sector_prices %>%
      dplyr::filter(date >= start_date)
  }

  if (nrow(sector_prices) == 0) {
    stop("No price data found for sector tickers")
  }

  # Calculate daily returns per ticker
  sector_prices %>%
    dplyr::arrange(ticker, date) %>%
    dplyr::group_by(ticker) %>%
    dplyr::mutate(daily_return = adjusted_close / dplyr::lag(adjusted_close) - 1) %>%
    dplyr::ungroup() %>%
    # Average daily return across tickers each day
    dplyr::group_by(date) %>%
    dplyr::summarize(
      avg_daily_return = mean(daily_return, na.rm = TRUE),
      n_stocks = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(date) %>%
    # Build sector index (starts at 1, compounds daily)
    dplyr::mutate(
      sector_index = cumprod(1 + tidyr::replace_na(avg_daily_return, 0)),
      sector_cumulative_return = sector_index - 1,
      sector_drawdown = drawdown_from_high(sector_index)
    )
}
