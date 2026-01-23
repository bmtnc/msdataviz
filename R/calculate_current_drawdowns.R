#' Calculate Current Drawdowns for Multiple Tickers
#'
#' Calculates the current drawdown (as of latest_date) for each ticker.
#'
#' @param price_data Data frame with ticker, date, adjusted_close columns
#' @param tickers Vector of ticker symbols to include
#' @param latest_date The date to calculate current drawdown as of
#' @return Numeric vector of current drawdowns (one per ticker)
#' @export
#' @keywords internal
calculate_current_drawdowns <- function(price_data, tickers, latest_date) {
  price_data %>%
    dplyr::filter(ticker %in% tickers) %>%
    dplyr::filter(!is.na(adjusted_close)) %>%
    dplyr::group_by(ticker) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      cummax_price = cummax(adjusted_close),
      drawdown = (adjusted_close - cummax_price) / cummax_price
    ) %>%
    dplyr::filter(date == latest_date) %>%
    dplyr::ungroup() %>%
    dplyr::pull(drawdown)
}
