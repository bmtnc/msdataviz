#' Prepare Rolling Beta Data
#'
#' Calculates rolling beta of a ticker vs its sector composite.
#'
#' @param price_data Data frame with columns: ticker, date, adjusted_close
#' @param target_ticker Character string for the target ticker
#' @param sector_tickers Character vector of tickers in the sector
#' @param roll_window Rolling window size in days (default: 252)
#' @return Data frame with columns: date, beta
#' @export
#' @keywords internal
prepare_rolling_beta_data <- function(
  price_data,
  target_ticker,
  sector_tickers,
  roll_window = 252L
) {
  avpipeline::validate_df_cols(
    price_data,
    c("ticker", "date", "adjusted_close")
  )
  avpipeline::validate_character_scalar(
    target_ticker,
    allow_empty = FALSE,
    name = "target_ticker"
  )
  avpipeline::validate_non_empty(sector_tickers, "sector_tickers")

  # Calculate log returns for target ticker
  ticker_returns <- price_data %>%
    dplyr::filter(ticker == target_ticker) %>%
    dplyr::filter(!is.na(adjusted_close)) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      ticker_return = log(adjusted_close / dplyr::lag(adjusted_close))
    ) %>%
    dplyr::select(date, ticker_return)

  # Calculate sector composite log returns (equal-weighted average)
  sector_returns <- price_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::filter(!is.na(adjusted_close)) %>%
    dplyr::arrange(ticker, date) %>%
    dplyr::group_by(ticker) %>%
    dplyr::mutate(
      log_return = log(adjusted_close / dplyr::lag(adjusted_close))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(
      sector_return = mean(log_return, na.rm = TRUE),
      .groups = "drop"
    )

  # Join ticker and sector returns
  return_data <- ticker_returns %>%
    dplyr::inner_join(sector_returns, by = "date") %>%
    dplyr::filter(!is.na(ticker_return), !is.na(sector_return)) %>%
    dplyr::arrange(date)

  if (nrow(return_data) < roll_window) {
    stop(
      "Insufficient data for rolling regression. Need at least ",
      roll_window,
      " observations."
    )
  }

  # Run rolling regression: ticker_return ~ sector_return
  roll_result <- roll::roll_lm(
    x = return_data$sector_return,
    y = return_data$ticker_return,
    width = roll_window
  )

  # Extract beta (slope coefficient)
  return_data %>%
    dplyr::mutate(beta = roll_result$coefficients[, "x1"]) %>%
    dplyr::filter(!is.na(beta)) %>%
    dplyr::select(date, beta)
}
